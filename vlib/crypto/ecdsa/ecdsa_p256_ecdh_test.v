module ecdsa

// These tests cover the P-256 ECDH addition made for net.quic's TLS 1.3
// key_share support (RFC 8446 §4.2.8.2, secp256r1 group) — see
// https://github.com/vlang/v/issues/27675. ECDH (shared-secret derivation) is
// a distinct operation from the ECDSA sign/verify already covered elsewhere
// in this package: same curve math, different EVP_PKEY_CTX operation.
//
// A NIST CAVP known-answer test (independent oracle, not just "both sides
// agree with each other") is intentionally NOT included here yet — per this
// project's fixture-provenance discipline (see vlib/net/quic/README.md), no
// unverified expected value should be checked in. It's tracked as part of
// Phase 2's from-scratch TLS 1.3 test vector authoring
// (vlib/net/quic/testdata/tls13_vectors/), which will need a verified P-256
// ECDH vector anyway for the key-share portion of the handshake fixtures.

fn test_p256_ecdh_two_sided_shared_secret_matches() {
	pub_a, priv_a := generate_key(nid: .prime256v1)!
	pub_b, priv_b := generate_key(nid: .prime256v1)!
	defer {
		priv_a.free()
		priv_b.free()
		pub_a.free()
		pub_b.free()
	}

	secret_ab := priv_a.derive_shared_secret(pub_b)!
	secret_ba := priv_b.derive_shared_secret(pub_a)!

	assert secret_ab.len > 0
	assert secret_ab == secret_ba
	// P-256's shared secret (the X-coordinate) is 32 bytes.
	assert secret_ab.len == 32
}

fn test_p256_public_key_uncompressed_roundtrip() {
	pub_a, priv_a := generate_key(nid: .prime256v1)!
	defer {
		priv_a.free()
		pub_a.free()
	}

	wire_bytes := pub_a.uncompressed_bytes()!
	// 0x04 prefix + 32-byte X + 32-byte Y for P-256.
	assert wire_bytes.len == 65
	assert wire_bytes[0] == 0x04

	mut reconstructed := PublicKey.from_uncompressed_bytes(wire_bytes, nid: .prime256v1)!
	defer {
		reconstructed.free()
	}
	assert pub_a.equal(reconstructed)
}

fn test_p256_ecdh_using_reconstructed_peer_public_key() {
	// Simulates the real net.quic usage: the peer's public key arrives on the
	// wire as raw bytes (from a TLS 1.3 key_share extension) and must be
	// reconstructed before deriving the shared secret.
	pub_a, priv_a := generate_key(nid: .prime256v1)!
	pub_b, priv_b := generate_key(nid: .prime256v1)!
	defer {
		priv_a.free()
		priv_b.free()
		pub_a.free()
		pub_b.free()
	}

	wire_b := pub_b.uncompressed_bytes()!
	mut reconstructed_b := PublicKey.from_uncompressed_bytes(wire_b, nid: .prime256v1)!
	defer {
		reconstructed_b.free()
	}

	secret_direct := priv_a.derive_shared_secret(pub_b)!
	secret_via_wire := priv_a.derive_shared_secret(reconstructed_b)!
	assert secret_direct == secret_via_wire
}

fn test_p256_from_uncompressed_bytes_rejects_compressed_point() {
	// Compressed points use a 0x02/0x03 prefix; only uncompressed (0x04) is
	// supported, matching what net.quic actually needs to parse from the wire.
	mut bad := []u8{len: 33}
	bad[0] = 0x02
	PublicKey.from_uncompressed_bytes(bad, nid: .prime256v1) or {
		assert err.msg().contains('uncompressed')
		return
	}
	assert false, 'expected an error for a compressed point'
}
