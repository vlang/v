module quic

import crypto.rand
import encoding.hex

// quiche_client_initial_capture is the raw bytes of the very first UDP
// datagram in a real quiche handshake capture (testdata/tls13_vectors/
// quiche_p256_handshake.pcap, frame 1: client 172.18.0.3:53012 -> server
// 172.18.0.2:4433) -- a single, non-coalesced Client Initial packet, 1200
// bytes including UDP-level padding. See testdata/tls13_vectors/README.md
// for full provenance (image digest, capture date, tooling). Extracted with
// a minimal standalone pcap parser (no Wireshark dependency needed here,
// since only raw UDP payload bytes are wanted, not TLS dissection) reading
// the SAME pcap file tls13_quiche_vector_test.v's TLS-layer vectors came
// from -- this is the plan's own suggested Phase 3 test strategy ("known-
// answer tests against Phase 2's captured Initial packets"), giving
// packet_protection.v/header_protection.v a REAL independent-implementation
// packet to decrypt, not just a self-round-trip.
const quiche_client_initial_capture = 'cd0000000110a38666b5dd3bb8eb36857f2e457adaf214d7af43d3ec88a36d745b07a58c8cd29bffa3158400412083db96d220e31b0a0c77efaee5a1d1f6fa78f754c7dae50bb0c2313b2d69a6ccb42f63137431eb6d044cbcb845170e42e1c34c3d311c155ab7b0e19cc7e5b2ed5d984bb6e161ca1c898e3594d9faf092ab6214d834c0ba075a08a6854eae840f6468bdaea98ca99758c40ea933ae2cbed77cdff84a02e5648260758dcbfcb4c1f6f42345404cc893808ecd003ccf8152877a4051fe8f88f3a9f308c7b2cc0854427f1a8ed9c704840452f31d7d1dd8a2dbcd3a8f4bb51954871f69fecda9a3aee950c3345a26a3f29efb495784d487a6bbfd1264acb114820f4870e2c5149b7682d102da2daff0dbb751231f7d30c8f65ce4694c9b169a07c9975fd8ff86d9d33f204bc51eea14ea60b635399269db1ed37f7476dd6ed9a54448402c5a98a0240000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000'

// RFC 9001 Appendix A.1 vectors, same values already used directly against
// hkdf_expand_label in initial_secrets_test.v -- duplicated here (rather than
// referenced) because V compiles each _test.v file as its own independent
// unit alongside the module's non-test files, so top-level consts are not
// shared across sibling _test.v files.
const rfc9001_client_dcid = '8394c8f03e515708'
const rfc9001_client_quic_key = '1f369613dd76d5467730efcbe3b1a22d'
const rfc9001_client_quic_iv = 'fa044b2f42a3fd3b46fb255c'
const rfc9001_client_quic_hp = '9f50449e04a0e810283a1e9933adedd2'
const rfc9001_server_quic_key = 'cf3a5331653c364c88f0f379b6067e37'
const rfc9001_server_quic_iv = '0ac1493ca1905853b0bba03e'
const rfc9001_server_quic_hp = 'c206b8d9b9f0f37644430b490eeaa314'

// real_capture_client_hello is the exact same ClientHello handshake message
// already declared as `quiche_vector_client_hello` in
// tls13_quiche_vector_test.v (obtained there via tshark's PDML-based TLS
// dissection of this SAME pcap file) -- duplicated here under a distinct
// name for the same file-isolation reason as the RFC constants above. Using
// the identical value obtained through a completely different extraction
// path (that file: Wireshark/tshark TLS dissection; this file: a standalone
// raw pcap/UDP parser with no TLS awareness at all, see
// testdata/tls13_vectors/README.md) is itself a small independent
// cross-check that neither extraction made the same mistake.
const real_capture_client_hello = '010001070303aae46c569ed2c383818039c15f2b9b2136b22d570eac6bce08f02caa38249a32000006130113021303010000d800000010000e00000b717569635f736572766572000a00080006001d001700180010001900170268330a68712d696e7465726f7008687474702f302e39000d00140012040308040401050308050501080606010201003300260024001d00202248ca6660c73714e4fcd54feab9983b52af11f84bd65fcc3b8d36a2c4e3723c002d00020101002b000302030400390048010480007530030245460404809896800504800f42400604800f42400704800f424008024064090240640a01030b01190c000f14d7af43d3ec88a36d745b07a58c8cd29bffa31584'

// test_packet_protection_decrypts_real_quiche_client_initial_capture derives
// Initial secrets purely from this real packet's own (always-visible,
// unprotected) DCID -- exactly as a real client/server would -- then removes
// header protection and AEAD-decrypts the payload. The decrypted plaintext
// is checked for containing the EXACT ClientHello bytes this session already
// independently verified in tls13_quiche_vector_test.v (byte-for-byte,
// cross-checked there against its own transcript/signature/MAC chain) --
// proving packet_protection.v + header_protection.v correctly unprotect a
// genuine third-party-produced packet, not just data this module produced
// and consumed itself. CRYPTO/PADDING frame parsing is Phase 4's job, so
// this checks containment of the known-good bytes within the decrypted
// payload rather than re-implementing frame parsing here.
fn test_packet_protection_decrypts_real_quiche_client_initial_capture() {
	raw := hex.decode(quiche_client_initial_capture)!

	long_header, header_len := parse_long_header(raw)!
	assert long_header.typ == .initial
	assert long_header.version == quic_v1

	// The UDP datagram (1200 bytes) is padded well past this single QUIC
	// packet's own end: `long_header.length` (the packet's own declared
	// pn+payload length) says the real packet ends far short of the
	// datagram's full size -- the rest is plain zero bytes appended outside
	// any packet, not part of the AEAD-protected payload. Feeding those
	// trailing bytes into the AEAD as if they were ciphertext (as an earlier
	// version of this test did) breaks tag verification, since the real
	// 16-byte tag would then be buried mid-buffer instead of at the end of
	// whatever's handed to unprotect_packet. Splitting a datagram at a
	// packet's own Length field is Phase 4's job (coalesce.v); this trims to
	// exactly one packet's bytes by hand since Phase 4 doesn't exist yet.
	mut packet := raw[..header_len + int(long_header.length)].clone()

	secrets := derive_initial_secrets(long_header.dcid)!
	keys := derive_packet_protection_keys(secrets.client)!

	unprotected := unprotect_packet(mut packet, header_len, .long, keys, none)!

	client_hello_bytes := hex.decode(real_capture_client_hello)!
	assert contains_subslice(unprotected.payload, client_hello_bytes)
}

// test_packet_protection_real_capture_rejects_tampered_payload flips one bit
// deep in the same real captured packet's ciphertext and confirms AEAD
// authentication fails cleanly (an error, never a panic or a silently wrong
// plaintext) -- the plan's explicitly requested negative test, run against
// genuine captured bytes rather than self-crafted ones.
fn test_packet_protection_real_capture_rejects_tampered_payload() {
	raw := hex.decode(quiche_client_initial_capture)!
	long_header, header_len := parse_long_header(raw)!
	// Trim to exactly this one packet's own bytes -- see the sibling
	// positive-path test's comment for why the full 1200-byte UDP datagram
	// (mostly trailing zero padding outside this packet) can't be handed to
	// unprotect_packet as-is.
	mut packet := raw[..header_len + int(long_header.length)].clone()
	packet[100] ^= 0x01 // byte 100 is well inside the real ciphertext region

	secrets := derive_initial_secrets(long_header.dcid)!
	keys := derive_packet_protection_keys(secrets.client)!

	unprotect_packet(mut packet, header_len, .long, keys, none) or { return }
	assert false, 'expected AEAD authentication to fail on tampered ciphertext'
}

// test_derive_packet_protection_keys_matches_rfc9001_initial_vectors chains
// derive_initial_secrets (already vector-tested in initial_secrets_test.v)
// into derive_packet_protection_keys and checks the result against the SAME
// RFC 9001 Appendix A.1 vectors that file already validates directly against
// hkdf_expand_label -- confirming this Phase 3 function is just correctly
// wiring three already-proven-correct derivations together, not scope creep
// re-deriving anything new.
fn test_derive_packet_protection_keys_matches_rfc9001_initial_vectors() {
	client_dcid := hex.decode(rfc9001_client_dcid)!
	secrets := derive_initial_secrets(client_dcid)!

	client_keys := derive_packet_protection_keys(secrets.client)!
	assert client_keys.key == hex.decode(rfc9001_client_quic_key)!
	assert client_keys.iv == hex.decode(rfc9001_client_quic_iv)!
	assert client_keys.hp == hex.decode(rfc9001_client_quic_hp)!

	server_keys := derive_packet_protection_keys(secrets.server)!
	assert server_keys.key == hex.decode(rfc9001_server_quic_key)!
	assert server_keys.iv == hex.decode(rfc9001_server_quic_iv)!
	assert server_keys.hp == hex.decode(rfc9001_server_quic_hp)!
}

fn truncate_packet_number(full_pn u64, pn_length int) []u8 {
	mut out := []u8{len: pn_length}
	for i in 0 .. pn_length {
		shift := u32((pn_length - 1 - i) * 8)
		out[i] = u8(full_pn >> shift)
	}
	return out
}

fn contains_subslice(haystack []u8, needle []u8) bool {
	if needle.len == 0 {
		return true
	}
	if needle.len > haystack.len {
		return false
	}
	for i in 0 .. haystack.len - needle.len + 1 {
		mut matched := true
		for j in 0 .. needle.len {
			if haystack[i + j] != needle[j] {
				matched = false
				break
			}
		}
		if matched {
			return true
		}
	}
	return false
}

fn test_protect_packet_then_unprotect_packet_round_trips_long_header_all_pn_lengths() {
	secret := rand.bytes(32)!
	keys := derive_packet_protection_keys(secret)!
	dcid := rand.bytes(8)!
	scid := rand.bytes(8)!
	payload := rand.bytes(50)!

	// (pn_length, packet_number) pairs, each packet number sized to fit
	// exactly within its paired encoded length so the round trip's decoded
	// value (with largest_pn: none, i.e. "first packet in this space") is
	// exactly recoverable without truncation ambiguity.
	cases := [[u64(1), u64(200)], [u64(2), u64(50000)], [u64(3), u64(12_000_000)],
		[u64(4), u64(3_000_000_000)]]

	for c in cases {
		pn_length := int(c[0])
		packet_number := c[1]

		h := QuicLongHeader{
			typ:     .initial
			version: quic_v1
			dcid:    dcid
			scid:    scid
			token:   []u8{}
			length:  u64(pn_length) + u64(payload.len) + 16 // + AEAD tag
		}
		mut header := encode_long_header(h, 0, u8(pn_length - 1))!
		header << truncate_packet_number(packet_number, pn_length)
		pn_offset := header.len - pn_length

		mut packet := protect_packet(header, .long, packet_number, pn_length, payload, keys)!
		unprotected := unprotect_packet(mut packet, pn_offset, .long, keys, none)!
		assert unprotected.packet_number == packet_number
		assert unprotected.payload == payload
	}
}

fn test_encrypt_packet_payload_rejects_iv_shorter_than_a_packet_number() {
	// QuicPacketProtectionKeys's fields are public and externally
	// constructible (not only ever produced by derive_packet_protection_keys),
	// so a malformed `iv` must fail cleanly here rather than let
	// packet_protection_nonce index out of bounds trying to XOR a full
	// 8-byte packet number into fewer than 8 bytes of IV.
	keys := QuicPacketProtectionKeys{
		key: rand.bytes(16)!
		iv:  rand.bytes(4)!
		hp:  rand.bytes(16)!
	}
	encrypt_packet_payload(keys, 1, []u8{len: 8}, []u8{len: 10}) or {
		assert err.msg().contains('IV')
		return
	}
	assert false, 'expected a too-short IV to be rejected'
}

fn test_protect_packet_then_unprotect_packet_round_trips_short_header() {
	secret := rand.bytes(32)!
	keys := derive_packet_protection_keys(secret)!
	dcid := rand.bytes(8)!
	payload := rand.bytes(40)!
	pn_length := 2
	packet_number := u64(12345)

	mut header := []u8{}
	header << u8(0x40 | (pn_length - 1))
	header << dcid
	header << truncate_packet_number(packet_number, pn_length)
	pn_offset := header.len - pn_length

	mut packet := protect_packet(header, .short, packet_number, pn_length, payload, keys)!
	unprotected := unprotect_packet(mut packet, pn_offset, .short, keys, none)!
	assert unprotected.packet_number == packet_number
	assert unprotected.payload == payload
}

fn test_unprotect_packet_rejects_tampered_ciphertext() {
	secret := rand.bytes(32)!
	keys := derive_packet_protection_keys(secret)!
	dcid := rand.bytes(8)!
	scid := rand.bytes(8)!
	payload := rand.bytes(50)!
	pn_length := 2
	packet_number := u64(7)

	h := QuicLongHeader{
		typ:     .initial
		version: quic_v1
		dcid:    dcid
		scid:    scid
		token:   []u8{}
		length:  u64(pn_length) + u64(payload.len) + 16
	}
	mut header := encode_long_header(h, 0, u8(pn_length - 1))!
	header << truncate_packet_number(packet_number, pn_length)
	pn_offset := header.len - pn_length

	mut packet := protect_packet(header, .long, packet_number, pn_length, payload, keys)!
	packet[packet.len - 1] ^= 0x01 // flip a bit inside the AEAD tag itself

	unprotect_packet(mut packet, pn_offset, .long, keys, none) or { return }
	assert false, 'expected AEAD authentication to fail on tampered ciphertext'
}

fn test_unprotect_packet_rejects_wrong_direction_keys() {
	secret := rand.bytes(32)!
	client_keys := derive_packet_protection_keys(secret)!
	// A different random secret stands in for the OTHER side's traffic
	// secret -- client and server secrets are always distinct PRKs (see
	// tls13_keyschedule.v), so using the wrong side's keys must fail closed,
	// not silently decrypt with the wrong material.
	other_secret := rand.bytes(32)!
	wrong_keys := derive_packet_protection_keys(other_secret)!

	dcid := rand.bytes(8)!
	scid := rand.bytes(8)!
	payload := rand.bytes(50)!
	pn_length := 2
	packet_number := u64(9)

	h := QuicLongHeader{
		typ:     .initial
		version: quic_v1
		dcid:    dcid
		scid:    scid
		token:   []u8{}
		length:  u64(pn_length) + u64(payload.len) + 16
	}
	mut header := encode_long_header(h, 0, u8(pn_length - 1))!
	header << truncate_packet_number(packet_number, pn_length)
	pn_offset := header.len - pn_length

	mut packet := protect_packet(header, .long, packet_number, pn_length, payload, client_keys)!
	unprotect_packet(mut packet, pn_offset, .long, wrong_keys, none) or { return }
	assert false, 'expected decryption with the wrong direction keys to fail'
}
