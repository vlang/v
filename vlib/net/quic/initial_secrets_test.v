module quic

import encoding.hex

// RFC 9001 Appendix A.1 ("Keys") test vectors — obtained directly from the
// raw RFC text (rfc-editor.org), not re-derived, so these assert against the
// spec's own published values rather than against this implementation's
// self-consistency.
const rfc9001_client_dcid = '8394c8f03e515708'
const rfc9001_client_initial_secret = 'c00cf151ca5be075ed0ebfb5c80323c42d6b7db67881289af4008f1f6c357aea'
const rfc9001_server_initial_secret = '3c199828fd139efd216c155ad844cc81fb82fa8d7446fa7d78be803acdda951b'
const rfc9001_client_quic_key = '1f369613dd76d5467730efcbe3b1a22d'
const rfc9001_client_quic_iv = 'fa044b2f42a3fd3b46fb255c'
const rfc9001_client_quic_hp = '9f50449e04a0e810283a1e9933adedd2'
const rfc9001_server_quic_key = 'cf3a5331653c364c88f0f379b6067e37'
const rfc9001_server_quic_iv = '0ac1493ca1905853b0bba03e'
const rfc9001_server_quic_hp = 'c206b8d9b9f0f37644430b490eeaa314'

fn test_derive_initial_secrets_matches_rfc9001_vector() {
	client_dcid := hex.decode(rfc9001_client_dcid)!
	secrets := derive_initial_secrets(client_dcid)!
	assert secrets.client == hex.decode(rfc9001_client_initial_secret)!
	assert secrets.server == hex.decode(rfc9001_server_initial_secret)!
}

// test_hkdf_expand_label_chained_derivation_matches_rfc9001_vector exercises
// hkdf_expand_label a second time, chained off the Phase 2a output, against
// the same RFC 9001 appendix's "quic key"/"quic iv"/"quic hp" derivations
// (which Phase 3's packet_protection.v will perform for real). Using these
// vectors here is legitimate extra coverage of `hkdf_expand_label` itself —
// three different output lengths (32/16/12 bytes) and both directions — not
// scope creep into Phase 3's packet-protection code, since no new public API
// is added for it.
fn test_hkdf_expand_label_chained_derivation_matches_rfc9001_vector() {
	client_dcid := hex.decode(rfc9001_client_dcid)!
	secrets := derive_initial_secrets(client_dcid)!

	client_key := hkdf_expand_label(secrets.client, 'quic key', []u8{}, 16)!
	client_iv := hkdf_expand_label(secrets.client, 'quic iv', []u8{}, 12)!
	client_hp := hkdf_expand_label(secrets.client, 'quic hp', []u8{}, 16)!
	assert client_key == hex.decode(rfc9001_client_quic_key)!
	assert client_iv == hex.decode(rfc9001_client_quic_iv)!
	assert client_hp == hex.decode(rfc9001_client_quic_hp)!

	server_key := hkdf_expand_label(secrets.server, 'quic key', []u8{}, 16)!
	server_iv := hkdf_expand_label(secrets.server, 'quic iv', []u8{}, 12)!
	server_hp := hkdf_expand_label(secrets.server, 'quic hp', []u8{}, 16)!
	assert server_key == hex.decode(rfc9001_server_quic_key)!
	assert server_iv == hex.decode(rfc9001_server_quic_iv)!
	assert server_hp == hex.decode(rfc9001_server_quic_hp)!
}

// test_derive_initial_secrets_is_sensitive_to_which_dcid_is_passed exercises
// the property Phase 9's `QuicConn` will depend on: this function must be
// keyed off whichever DCID it's given, so that calling it with
// `original_dcid` vs. `current_dcid` (post-Retry) produces genuinely
// different secrets — proving the distinction matters, not just that it's
// documented. `QuicConn` itself doesn't exist until Phase 9; this test
// stands in for that call site with two literal DCIDs.
fn test_derive_initial_secrets_is_sensitive_to_which_dcid_is_passed() {
	original_dcid := hex.decode(rfc9001_client_dcid)!
	mut current_dcid := original_dcid.clone()
	current_dcid[0] ^= 0xff // simulate a Retry-assigned different DCID

	secrets_from_original := derive_initial_secrets(original_dcid)!
	secrets_from_current := derive_initial_secrets(current_dcid)!

	assert secrets_from_original.client != secrets_from_current.client
	assert secrets_from_original.server != secrets_from_current.server
	// Sanity: re-deriving from the same (original) DCID is deterministic.
	secrets_from_original_again := derive_initial_secrets(original_dcid)!
	assert secrets_from_original.client == secrets_from_original_again.client
	assert secrets_from_original.server == secrets_from_original_again.server
}

fn test_derive_initial_secrets_client_and_server_secrets_differ() {
	client_dcid := hex.decode(rfc9001_client_dcid)!
	secrets := derive_initial_secrets(client_dcid)!
	assert secrets.client != secrets.server
}

fn test_hkdf_expand_label_rejects_negative_length() {
	hkdf_expand_label([]u8{len: 32}, 'client in', []u8{}, -1) or {
		assert err.msg().contains('out of range')
		return
	}
	assert false, 'expected an error for negative length'
}

fn test_hkdf_expand_label_rejects_length_over_u16_max() {
	hkdf_expand_label([]u8{len: 32}, 'client in', []u8{}, 0x1_0000) or {
		assert err.msg().contains('out of range')
		return
	}
	assert false, 'expected an error for length > 0xffff'
}

fn test_hkdf_expand_label_rejects_oversized_label() {
	// "tls13 " (6 bytes) + a 250-byte label = 256 bytes, over the 255-byte
	// wire limit for HkdfLabel.label.
	oversized_label := 'x'.repeat(250)
	hkdf_expand_label([]u8{len: 32}, oversized_label, []u8{}, 32) or {
		assert err.msg().contains('out of range')
		return
	}
	assert false, 'expected an error for an oversized label'
}

fn test_hkdf_expand_label_rejects_oversized_context() {
	oversized_context := []u8{len: 256}
	hkdf_expand_label([]u8{len: 32}, 'client in', oversized_context, 32) or {
		assert err.msg().contains('out of range')
		return
	}
	assert false, 'expected an error for an oversized context'
}

fn test_hkdf_expand_label_accepts_minimum_label_length() {
	// "tls13 " is exactly 6 bytes; a 1-byte label brings the full label to
	// the 7-byte wire minimum for HkdfLabel.label<7..255>.
	out := hkdf_expand_label([]u8{len: 32}, 'x', []u8{}, 32)!
	assert out.len == 32
}
