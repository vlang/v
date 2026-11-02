// Copyright (c) 2026 blackshirt.
//
// Adversarial ECDHE key-share tests: compute_shared_secret must reject peer
// shares that would hand us a low-entropy or invalid shared secret —
// wrong sizes, the all-zero and small-order X25519 results (RFC 7748 §6.1),
// and compressed P-256 points (RFC 8446 §4.2.8.2).
module handshake

import crypto.ecdsa
import crypto.subtle
import tls.pki
import x.crypto.curve25519

// empty_store_validator builds a Validator over an empty trust store; none of
// these tests reach chain verification. V compiles each _test.v file on its own,
// so this file carries its own fixture instead of reaching into a sibling suite.
fn empty_store_validator() pki.Validator {
	return pki.new_validator(pki.new_trust_store())
}

// core_with_group builds a HandshakeCore whose ephemeral share lives on the
// requested group — the exact precondition compute_shared_secret checks.
fn core_with_group(group NamedGroup) !HandshakeCore {
	return new_handshake_core('example.com', HandshakeOptions{
		validator:       empty_store_validator()
		key_share_group: group
	}, false)
}

// test_rejects_all_zero_x25519_share checks the §4.2.8.2 pre-backend gate: an
// all-zero peer u-coordinate would yield an all-zero secret and must surface as
// illegal_parameter, not a backend result.
fn test_rejects_all_zero_x25519_share() ! {
	mut h := core_with_group(.x25519)!
	mut rejected := false
	h.compute_shared_secret(KeyShareEntry{
		group:        .x25519
		key_exchange: []u8{len: 32, init: 0x00}
	}) or {
		assert err.msg().contains('all-zero')
		assert h.fail_alert == .illegal_parameter
		// The classified failure must release the ephemeral private key (§7.5).
		assert h.priv_x25519 == unsafe { nil }
		rejected = true
	}
	assert rejected, 'an all-zero X25519 share must be rejected'
}

// test_rejects_small_order_x25519_shares checks the RFC 7748 §6.1 results:
// peer u = 1 and u = p drive any private key to a constant secret, so both must
// be rejected even though the backend computes them without error. The shares
// are built in the little-endian wire order X25519 itself uses.
fn test_rejects_small_order_x25519_shares() ! {
	mut u_one := []u8{len: 32} // u = 1 -> the shared secret is 1
	u_one[0] = 1
	mut u_p := []u8{len: 32, init: 0xff} // u = 2^255-19, which reduces to 0 mod p
	u_p[0] = 0xed
	u_p[31] = 0x7f
	// u = 1 is a canonical encoding, so it reaches the exchange and lands on the
	// pinned small-order result; u = p fails the RFC 7748 §5 canonical gate first
	// — rejecting the non-canonical encoding outright, before the scalar
	// multiplication, makes the refusal deterministic instead of backend-dependent.
	// Both paths are the peer's fault and must pin the same alert.
	peers := [u_one, u_p]
	expected_msgs := ['small-order', 'not a canonical encoding']
	mut rejections := 0
	for i, share in peers {
		// A classified key_share rejection is terminal: classify_failure releases the
		// ephemeral private key (RFC 8446 §7.5), so each peer share must be exercised
		// against a fresh core rather than a torn-down one.
		mut h := core_with_group(.x25519)!
		mut rejected := false
		h.compute_shared_secret(KeyShareEntry{
			group:        .x25519
			key_exchange: share
		}) or {
			assert err.msg().contains(expected_msgs[i])
			assert h.fail_alert == .illegal_parameter
			rejected = true
		}
		assert rejected, 'a small-order X25519 share must be rejected'
		if rejected {
			rejections += 1
		}
	}
	assert rejections == peers.len
}

// test_rejects_wrong_size_key_shares pins the §4.2.8.2 size gate ahead of any
// cryptographic processing.
fn test_rejects_wrong_size_key_shares() ! {
	mut h := core_with_group(.x25519)!
	mut rejected := false
	h.compute_shared_secret(KeyShareEntry{
		group:        .x25519
		key_exchange: [u8(0xaa)]
	}) or {
		assert err.msg().contains('expected 32')
		assert h.fail_alert == .illegal_parameter
		rejected = true
	}
	assert rejected, 'a mis-sized X25519 share must be rejected'

	mut h2 := core_with_group(.secp256r1)!
	rejected = false
	h2.compute_shared_secret(KeyShareEntry{
		group:        .secp256r1
		key_exchange: []u8{len: 64, init: 0x04}
	}) or {
		assert err.msg().contains('expected 65')
		assert h2.fail_alert == .illegal_parameter
		rejected = true
	}
	assert rejected, 'a mis-sized P-256 share must be rejected'
}

// test_rejects_compressed_p256_share checks that only the uncompressed 0x04
// encoding is accepted (§4.2.8.2); 0x02/0x03 compressed points are refused
// before point decoding.
fn test_rejects_compressed_p256_share() ! {
	mut h := core_with_group(.secp256r1)!
	prefixes := [u8(0x02), 0x03]
	for i in 0 .. prefixes.len {
		mut share := [prefixes[i]]
		for _ in 0 .. 64 { // the 64 affine coordinates that follow the encoding prefix
			share << u8(0x11)
		}
		assert share.len == 65, 'a compressed share must be 1+64 bytes, or the size gate fires first'
		mut rejected := false
		h.compute_shared_secret(KeyShareEntry{
			group:        .secp256r1
			key_exchange: share
		}) or {
			assert err.msg().contains('uncompressed point')
			assert h.fail_alert == .illegal_parameter
			rejected = true
		}
		assert rejected, 'a compressed P-256 share (0x${prefixes[i]:02x}) must be refused'
	}
}

// test_rejects_invalid_uncompressed_p256_share checks that a correctly sized,
// uncompressed encoding of an off-curve point is treated as a peer error.
fn test_rejects_invalid_uncompressed_p256_share() ! {
	mut h := core_with_group(.secp256r1)!
	mut share := []u8{len: 65, init: 0}
	share[0] = 0x04
	mut rejected := false
	h.compute_shared_secret(KeyShareEntry{
		group:        .secp256r1
		key_exchange: share
	}) or {
		assert h.fail_alert == .illegal_parameter
		rejected = true
	}
	assert rejected, 'an off-curve P-256 share must be rejected'
}

// test_valid_x25519_share_is_accepted guards against an over-tight rejection:
// a genuine random peer share must produce its matching shared secret.
fn test_valid_x25519_share_is_accepted() ! {
	mut h := core_with_group(.x25519)!
	peer := curve25519.PrivateKey.new()!
	peer_pub := peer.public_key()!
	share := peer_pub.bytes()!
	secret := h.compute_shared_secret(KeyShareEntry{
		group:        .x25519
		key_exchange: share
	})!
	own_pub := h.priv_x25519.public_key()!
	own_pub_bytes := own_pub.bytes()!
	expected := peer.x25519(own_pub_bytes)!
	assert subtle.constant_time_compare(secret, expected) == 1
}

// test_rejects_p256_identity_point pins the explicit point-at-infinity refusal
// (0x04 || 0^32 || 0^32): the identity contributes nothing to the shared secret,
// so the encoding is rejected deterministically rather than via the backend.
fn test_rejects_p256_identity_point() ! {
	mut h := core_with_group(.secp256r1)!
	mut rejected := false
	mut identity := [u8(0x04)]
	identity << []u8{len: 64} // 0x04 || 0^32 || 0^32
	h.compute_shared_secret(KeyShareEntry{
		group:        .secp256r1
		key_exchange: identity
	}) or {
		assert err.msg().contains('point at infinity')
		assert h.fail_alert == .illegal_parameter
		rejected = true
	}
	assert rejected, 'the P-256 identity point must be rejected'
}

// test_rejects_non_canonical_x25519_encodings pins the RFC 7748 §5 gate: encodings
// at or above the field prime reduce mod p and would silently duplicate a smaller
// share, so they must be refused in-module with a named error, not left to the
// backend. u = p and u = p+1 both exceed the field.
fn test_rejects_non_canonical_x25519_encodings() ! {
	mut at_p := x25519_p.clone()
	assert at_p.len == 32
	mut over_p := x25519_p.clone()
	over_p[0] = 0xee // p+1: the least significant little-endian byte increments without carry
	peers := [at_p, over_p]
	for share in peers {
		// A classified rejection releases the ephemeral key, so each share needs a fresh core.
		mut h := core_with_group(.x25519)!
		mut rejected := false
		h.compute_shared_secret(KeyShareEntry{
			group:        .x25519
			key_exchange: share
		}) or {
			assert err.msg().contains('not a canonical encoding')
			assert h.fail_alert == .illegal_parameter
			assert h.priv_x25519 == unsafe { nil }
			rejected = true
		}
		assert rejected, 'a non-canonical X25519 encoding must be rejected'
	}
}

// test_accepts_maximal_canonical_x25519_u locks the gate against an off-by-one
// false rejection: u = p-1 is canonical, so it must reach the exchange itself.
// p-1 is a small-order point, so the share is still refused — but by the
// small-order path (peer fault), never as an internal_error.
fn test_accepts_maximal_canonical_x25519_u() ! {
	mut h := core_with_group(.x25519)!
	h.compute_shared_secret(KeyShareEntry{
		group:        .x25519
		key_exchange: x25519_secret_p_minus_1.clone()
	}) or {
		// Either the backend refuses the low-order input or it returns the small-order
		// result; both classify as the peer's fault, and neither may be a nil/internal path.
		assert err.msg().contains('small-order')
		assert h.fail_alert == .illegal_parameter
		return
	}
	assert false, 'u = p-1 is a small-order contribution and must never be accepted'
}

// test_rejects_non_canonical_p256_coordinates pins the SEC1 3.2.3.1 field-element
// gate: a coordinate equal to the prime is not a field element even though it has
// the right bit length, and the refusal must name the coordinate rule rather than
// surfacing a generic decode failure.
fn test_rejects_non_canonical_p256_coordinates() ! {
	// Generator Y, a canonical nonzero field element (SEC2 2.4.2, big-endian), so the
	// only fault in these shares is the non-canonical coordinate under test.
	gy := [u8(0x4f), 0xe3, 0x42, 0xe2, 0xfe, 0x1a, 0x7f, 0x9b, 0x8e, 0xe7, 0xeb, 0x4a, 0x7c, 0x0f,
		0x9e, 0x16, 0x2b, 0xce, 0x33, 0x57, 0x6b, 0x31, 0x5e, 0xce, 0xeb, 0xbb, 0xfd, 0xba, 0x71,
		0x7d, 0x4a, 0x93]
	mut x_at_p := [u8(0x04)]
	x_at_p << p256_p.clone()
	x_at_p << gy
	mut y_at_p := [u8(0x04)]
	y_at_p << gy
	y_at_p << p256_p.clone()
	for share in [x_at_p, y_at_p] {
		assert share.len == 65, 'or the size gate fires first and the test misses its target'
		mut h := core_with_group(.secp256r1)!
		mut rejected := false
		h.compute_shared_secret(KeyShareEntry{
			group:        .secp256r1
			key_exchange: share
		}) or {
			assert err.msg().contains('not canonical or is zero')
			assert h.fail_alert == .illegal_parameter
			assert h.priv_p256 == none
			rejected = true
		}
		assert rejected, 'a P-256 coordinate equal to p must be rejected'
	}
}

// test_rejects_zero_p256_coordinate checks the second half of the same SEC1 rule:
// X = 0 with a canonical nonzero Y passes the bound gate and must be refused by the
// zero-coordinate check. (A zero-coordinate pair is never a valid affine point; the
// identity *encoding* has its own earlier check, so this reaches the new gate.)
fn test_rejects_zero_p256_coordinate() ! {
	mut share := [u8(0x04)]
	share << []u8{len: 32, init: 0} // X = 0
	share << [u8(0x00)]
	share << []u8{len: 30, init: 0}
	share << [u8(0x01)] // Y = 1: canonical and nonzero, so only the zero rule can fire
	assert share.len == 65
	mut h := core_with_group(.secp256r1)!
	mut rejected := false
	h.compute_shared_secret(KeyShareEntry{
		group:        .secp256r1
		key_exchange: share
	}) or {
		assert err.msg().contains('not canonical or is zero')
		assert h.fail_alert == .illegal_parameter
		rejected = true
	}
	assert rejected, 'a zero P-256 coordinate must be rejected'
}

// test_valid_p256_share_is_accepted guards every new P-256 gate against over-tight
// rejection: a genuine random peer share must produce its matching shared secret,
// mirroring test_valid_x25519_share_is_accepted.
fn test_valid_p256_share_is_accepted() ! {
	mut h := core_with_group(.secp256r1)!
	mut peer := ecdsa.PrivateKey.new(nid: .prime256v1)!
	peer_pub := peer.public_key()!
	share := peer_pub.uncompressed_bytes()!
	secret := h.compute_shared_secret(KeyShareEntry{
		group:        .secp256r1
		key_exchange: share
	})!
	// public_key() returns a Result, which a `!` function cannot propagate, and
	// the Option unwrap leaves a non-addressable value: take the `or` form and
	// bind it mut, matching the `mut peer`/`peer_pub` idiom above.
	mut own := h.priv_p256 or { panic('missing P-256 private key') }
	own_pub := own.public_key() or { panic('public key derivation: ${err.msg()}') }
	expected := peer.derive_shared_secret(own_pub)!
	assert subtle.constant_time_compare(secret, expected) == 1
}
