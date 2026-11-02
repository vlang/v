// Copyright (c) 2026 blackshirt.
//
// key_exchange.v owns the ephemeral ECDHE half of the handshake: per-group key
// generation, the §4.2.8.2 share-size gate, peer-share validation and secret
// derivation, including the RFC 7748 §6.1 small-order bounds. Its methods attach to
// HandshakeCore, whose private-key holders are declared in state_machine.v.
module handshake

import crypto.ecdsa
import crypto.subtle
import x.crypto.curve25519

// key_exchange_len is the §4.2.8.2 key_exchange size per group (x25519 32;
// secp256r1 65 = 0x04||X||Y): one source for the peer gate and ClientHello builder.
fn key_exchange_len(g NamedGroup) int {
	return match g {
		.x25519 { 32 }
		.secp256r1 { 65 }
	}
}

// reset_key_share creates a fresh ephemeral private key for the requested group,
// zeroing any previous share first.
fn (mut h HandshakeCore) reset_key_share(group NamedGroup) ! {
	h.ecdhe_group = group
	// Only the key for the selected group stays live.
	h.priv_x25519 = unsafe { nil }
	h.priv_p256 = none
	match group {
		.x25519 {
			h.priv_x25519 = curve25519.PrivateKey.new()!
		}
		.secp256r1 {
			h.priv_p256 = ecdsa.PrivateKey.new(nid: .prime256v1)!
		}
	}
}

// public_key_bytes serializes the public half of the current ephemeral key in
// the wire form its group mandates (RFC 8446 §4.2.8.2).
fn (mut h HandshakeCore) public_key_bytes() ![]u8 {
	match h.ecdhe_group {
		.x25519 {
			// Same explicit guard as the P-256 leg: calling through a nil reference
			// would segfault instead of yielding a named error.
			if h.priv_x25519 == unsafe { nil } {
				return error('handshake: missing X25519 private key')
			}
			pub_key := h.priv_x25519.public_key()!
			return pub_key.bytes()!
		}
		.secp256r1 {
			priv := h.priv_p256 or { return error('handshake: missing P-256 private key') }
			pub_key := priv.public_key()!
			return pub_key.uncompressed_bytes()!
		}
	}
}

// The three small-order X25519 shared-secret results of RFC 7748 §6.1, in the
// little-endian wire encoding: the identity, u = 1, and u = p-1 = 2^255-20.
const x25519_secret_zero = [u8(0), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

const x25519_secret_one = [u8(1), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

const x25519_secret_p_minus_1 = [u8(0xec), 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0x7f]

// x25519_p is the Curve25519 field prime 2^255-19 in the little-endian wire
// encoding (RFC 7748 §5): p-1 above with byte 0 corrected from 0xec to 0xed.
const x25519_p = [u8(0xed), 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0x7f]

// p256_p is the P-256 field prime in big-endian affine order (SEC2 2.4.1),
// matching the byte order of uncompressed key_share coordinates.
const p256_p = [u8(0xff), 0xff, 0xff, 0xff, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff]

// is_canonical_x25519_u reports whether a peer u-coordinate encodes a field
// element below p. RFC 7748 §5 recommends rejecting larger encodings: they
// reduce mod p and would silently duplicate a smaller share, so the gate runs
// before the backend decides whether to notice. Borrow arithmetic is required —
// plain lexicographic byte comparison is wrong because a carry/borrow propagates
// across byte boundaries — but the input is public, so the branchy loop is fine.
// The caller's §4.2.8.2 size gate guarantees exactly 32 little-endian bytes.
@[direct_array_access]
fn is_canonical_x25519_u(u []u8) bool {
	mut borrow := 0
	for i in 0 .. 32 {
		// Little-endian: the borrow flows from each byte into the next more
		// significant one; an explicit flag avoids relying on negative-shift semantics.
		diff := int(u[i]) - int(x25519_p[i]) - borrow
		if diff < 0 {
			borrow = 1
		} else {
			borrow = 0
		}
	}
	// A final borrow means u - p underflowed, i.e. the encoding was below p.
	return borrow == 1
}

// is_in_p256_field reports whether a 32-byte big-endian affine coordinate is
// below the field prime (SEC1 3.2.3.1 requires field elements, not merely
// bit-length-fitting ones). Same borrow reasoning as is_canonical_x25519_u;
// here the loop runs most-significant to least so the borrow flows downward.
// The caller checks the slice length.
@[direct_array_access]
fn is_in_p256_field(v []u8) bool {
	mut borrow := 0
	for i in 0 .. 32 {
		// Big-endian: walk from the least significant byte so the borrow flows
		// downward across byte boundaries.
		diff := int(v[31 - i]) - int(p256_p[31 - i]) - borrow
		if diff < 0 {
			borrow = 1
		} else {
			borrow = 0
		}
	}
	// A final borrow means v - p underflowed, i.e. the coordinate was below p.
	return borrow == 1
}

// is_x25519_small_order reports whether a finished X25519 shared secret landed
// in one of the three small-order results (RFC 7748 §6.1); reject all three.
@[direct_array_access]
fn is_x25519_small_order(secret []u8) bool {
	if subtle.constant_time_compare(secret, x25519_secret_zero) == 1 {
		return true
	}
	if subtle.constant_time_compare(secret, x25519_secret_one) == 1 {
		return true
	}
	return subtle.constant_time_compare(secret, x25519_secret_p_minus_1) == 1
}

// compute_shared_secret performs the ECDHE exchange against the server's key
// share, returning the raw shared secret that seeds the key schedule.
fn (mut h HandshakeCore) compute_shared_secret(server_share KeyShareEntry) ![]u8 {
	// §4.2.8.2 fixes key_exchange size per group; gate it here so a wrong size
	// yields a named protocol error.
	expected_len := key_exchange_len(server_share.group)
	if server_share.key_exchange.len != expected_len {
		return h.classify_failure(.illegal_parameter, 'handshake: ${server_share.group} key_share is ${server_share.key_exchange.len} bytes, expected ${expected_len}')
	}
	match server_share.group {
		.x25519 {
			if h.priv_x25519 == unsafe { nil } {
				return error('handshake: missing X25519 private key')
			}
			// RFC 7748 §5 public-key validation, done in-module so the refusal is
			// deterministic instead of trusting the backend to notice u >= p.
			if !is_canonical_x25519_u(server_share.key_exchange) {
				return h.classify_failure(.illegal_parameter, 'handshake: X25519 key_share u-coordinate is not a canonical encoding (illegal_parameter)')
			}
			// §4.2.8.2 & RFC 7748 §6.1: an all-zero peer u-coordinate would yield an
			// all-zero secret; gate before the backend.
			if subtle.constant_time_compare(server_share.key_exchange, []u8{len: 32, init: 0}) == 1 {
				return h.classify_failure(.illegal_parameter, 'handshake: computed X25519 shared secret is all-zero value (illegal_parameter)')
			}
			// The backend runs its own low-order and zero-result checks, so a
			// small-order contribution can fail inside the scalar multiplication and
			// never reach the result comparison below. Classify every failure of this
			// exchange as the peer's fault: RFC 8446 §4.2.8.2 makes a bad key_share
			// illegal_parameter, and pinning it keeps the sent alert off internal_error.
			secret := h.priv_x25519.x25519(server_share.key_exchange) or {
				return h.classify_failure(.illegal_parameter, 'handshake: X25519 key_share is a small-order contribution (illegal_parameter)')
			}
			// Defense in depth: reject every small-order result, not just the all-zero one.
			if is_x25519_small_order(secret) {
				return h.classify_failure(.illegal_parameter, 'handshake: computed X25519 shared secret is a small-order value (illegal_parameter)')
			}
			return secret
		}
		.secp256r1 {
			// Only the uncompressed encoding is legal in TLS 1.3 (§4.2.8.2); compressed shares start 0x02/0x03.
			// A malformed share is the sender's fault, so it must pin the alert.
			if server_share.key_exchange[0] != 0x04 {
				return h.classify_failure(.illegal_parameter, 'handshake: P-256 key_share is not an uncompressed point (0x04 prefix)')
			}
			// 0x04 || 0^32 || 0^32 encodes the point at infinity: the exchange would
			// contribute nothing to the secret. Reject the identity encoding itself,
			// deterministically, instead of trusting the backend to refuse it.
			if subtle.constant_time_compare(server_share.key_exchange[1..server_share.key_exchange.len],
				[]u8{len: 64, init: 0}) == 1 {
				return h.classify_failure(.illegal_parameter, 'handshake: P-256 key_share is the point at infinity (illegal_parameter)')
			}
			// SEC1 3.2.3.1 field-element validation, done in-module: each coordinate
			// must be canonical (below p) and nonzero. On-curve membership remains the
			// decoder's job below; these bounds must not depend on the backend checking.
			if !is_in_p256_field(server_share.key_exchange[1..33])
				|| !is_in_p256_field(server_share.key_exchange[33..65]) {
				return h.classify_failure(.illegal_parameter, 'handshake: P-256 key_share coordinate is not canonical or is zero (illegal_parameter)')
			}
			if subtle.constant_time_compare(server_share.key_exchange[1..33], []u8{len: 32, init: 0}) ==
				1 || subtle.constant_time_compare(server_share.key_exchange[33..65],
				[]u8{len: 32, init: 0}) == 1 {
				return h.classify_failure(.illegal_parameter, 'handshake: P-256 key_share coordinate is not canonical or is zero (illegal_parameter)')
			}
			priv := h.priv_p256 or { return error('handshake: missing P-256 private key') }
			// A point that does not decode is the sender's fault too, so pin the alert
			// instead of letting a backend message surface as internal_error.
			peer := ecdsa.PublicKey.from_uncompressed_bytes(server_share.key_exchange,
				nid: .prime256v1
			) or {
				return h.classify_failure(.illegal_parameter, 'handshake: P-256 key_share is not a valid uncompressed point on the curve (illegal_parameter)')
			}
			secret := priv.derive_shared_secret(peer) or {
				return h.classify_failure(.illegal_parameter, 'handshake: P-256 ECDH operation failed')
			}
			if subtle.constant_time_compare(secret, []u8{len: secret.len, init: 0}) == 1 {
				return h.classify_failure(.illegal_parameter, 'handshake: computed P-256 shared secret is all-zero value (illegal_parameter)')
			}
			return secret
		}
	}
	// Unreachable: the match above is exhaustive over NamedGroup.
	return error('handshake: unsupported key_share group ${server_share.group}')
}
