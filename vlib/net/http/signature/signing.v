// Sign / verify primitives - the part that actually consumes the
// signature base. Each branch wraps the corresponding `vlib/crypto`
// routine (ed25519, ecdsa, hmac) and converts between RFC 9421's
// preferred formats and what the V crypto modules expose:
//
//   * Ed25519: raw 32-byte seed in / out (matches RFC 8032).
//   * ECDSA:   raw R‖S concatenation (RFC 9421 §3.3.4); V's `ecdsa`
//              module talks DER, so we convert at the boundary.
//   * HMAC:    raw bytes out (RFC 9421 §3.3.3).
module signature

import crypto.ecdsa
import crypto.ed25519
import crypto.hmac
import crypto.sha256

const ec_p256_size = 32
const ec_p384_size = 48

// sign_base computes the signature of `base` using `key`.
// Returns the raw signature bytes (not yet base64-encoded).
fn sign_base(base []u8, key Key) ![]u8 {
	if !key.is_private && !key.algorithm.is_mac() {
		return MalformedMessage{
			reason: 'sign requires a private key for ${key.algorithm.name()}'
		}
	}
	return match key.algorithm {
		.hmac_sha256 { hmac.new(key.bytes, base, sha256.sum, sha256.block_size) }
		.ed25519 { ed25519_sign(base, key.bytes)! }
		.ecdsa_p256_sha256 { ecdsa_sign(base, key, ec_p256_size, .prime256v1)! }
		.ecdsa_p384_sha384 { ecdsa_sign(base, key, ec_p384_size, .secp384r1)! }
	}
}

// verify_base checks that `signature` is valid over `base` under `key`.
// Returns a typed `VerificationFailed` rather than `false`+nil so
// callers can distinguish "signature invalid" from "verification
// machinery itself failed".
fn verify_base(base []u8, signature []u8, key Key, label string) ! {
	ok := match key.algorithm {
		.hmac_sha256 {
			expected := hmac.new(key.bytes, base, sha256.sum, sha256.block_size)
			hmac.equal(signature, expected)
		}
		.ed25519 {
			ed25519_verify(base, signature, key)!
		}
		.ecdsa_p256_sha256 {
			ecdsa_verify(base, signature, key, ec_p256_size, .prime256v1)!
		}
		.ecdsa_p384_sha384 {
			ecdsa_verify(base, signature, key, ec_p384_size, .secp384r1)!
		}
	}

	if !ok {
		return VerificationFailed{
			label: label
		}
	}
}

fn ed25519_sign(base []u8, seed []u8) ![]u8 {
	if seed.len != ed25519.seed_size {
		return MalformedMessage{
			reason: 'Ed25519 seed must be ${ed25519.seed_size} bytes, got ${seed.len}'
		}
	}
	priv := ed25519.new_key_from_seed(seed)
	return priv.sign(base)
}

fn ed25519_verify(base []u8, sig []u8, key Key) !bool {
	pub_key := if key.is_private {
		ed25519.new_key_from_seed(key.bytes).public_key()
	} else {
		ed25519.PublicKey(key.bytes.clone())
	}
	return ed25519.verify(pub_key, base, sig)
}

fn ecdsa_sign(base []u8, key Key, coord_size int, curve ecdsa.Nid) ![]u8 {
	// Raw layout for an ECDSA private Key is x || y || d (each
	// `coord_size` bytes). V's ecdsa module derives x and y from d
	// alone, so we only need d to build the EVP key.
	if key.bytes.len != coord_size * 3 {
		return MalformedMessage{
			reason: 'ECDSA private key must be ${coord_size * 3} bytes (x||y||d), got ${key.bytes.len}'
		}
	}
	d := key.bytes[coord_size * 2..coord_size * 3]
	priv := ecdsa.new_key_from_seed(d, nid: curve, fixed_size: true) or {
		return MalformedMessage{
			reason: 'ECDSA new_key_from_seed failed: ${err.msg()}'
		}
	}
	defer {
		priv.free()
	}
	der := priv.sign(base) or {
		return MalformedMessage{
			reason: 'ECDSA sign failed: ${err.msg()}'
		}
	}
	return der_to_raw(der, coord_size)
}

fn ecdsa_verify(base []u8, sig []u8, key Key, coord_size int, curve ecdsa.Nid) !bool {
	if sig.len != coord_size * 2 {
		return VerificationFailed{}
	}
	pub_key := build_ecdsa_public(key, coord_size, curve)!
	defer {
		pub_key.free()
	}
	der := raw_to_der(sig, coord_size)
	return pub_key.verify(base, der) or {
		// V's ecdsa.verify returns an error for malformed DER but
		// success/false for "signature does not match". We mapped
		// both to VerificationFailed at the call site, so swallow
		// the error here and return false.
		false
	}
}

fn build_ecdsa_public(key Key, coord_size int, curve ecdsa.Nid) !ecdsa.PublicKey {
	// For a private Key we build via the seed path (V's ecdsa derives
	// x, y from d). For a public Key we hand-craft a SubjectPublicKeyInfo
	// DER from the supplied (x, y) - this is the only place where this
	// module talks raw ASN.1 DER.
	if key.is_private {
		if key.bytes.len != coord_size * 3 {
			return MalformedMessage{
				reason: 'ECDSA private key must be ${coord_size * 3} bytes'
			}
		}
		d := key.bytes[coord_size * 2..coord_size * 3]
		priv := ecdsa.new_key_from_seed(d, nid: curve, fixed_size: true)!
		pub_key := priv.public_key()!
		priv.free()
		return pub_key
	}
	if key.bytes.len != coord_size * 2 {
		return MalformedMessage{
			reason: 'ECDSA public key must be ${coord_size * 2} bytes (x||y)'
		}
	}
	x := key.bytes[..coord_size]
	y := key.bytes[coord_size..coord_size * 2]
	spki := build_ec_spki(x, y, curve)
	return ecdsa.pubkey_from_bytes(spki)!
}

// der_to_raw extracts (R, S) from a DER-encoded ECDSA signature and
// returns R‖S left-padded to `coord_size` bytes each. RFC 3279 §2.2.3
// shape is: SEQUENCE { INTEGER R, INTEGER S }.
fn der_to_raw(der []u8, coord_size int) ![]u8 {
	if der.len < 8 || der[0] != 0x30 {
		return MalformedMessage{
			reason: 'ECDSA signature: malformed DER (no SEQUENCE)'
		}
	}
	mut idx, _ := read_der_length(der, 1)!
	r, idx2 := read_der_integer(der, idx)!
	s, _ := read_der_integer(der, idx2)!
	mut out := []u8{len: coord_size * 2}
	pad_into(mut out, 0, r, coord_size)
	pad_into(mut out, coord_size, s, coord_size)
	return out
}

// raw_to_der packs R‖S back into the SEQUENCE form `ecdsa.verify`
// expects. Both halves are sign-extended (a 0x00 prefix is added when
// the high bit is set) so they remain non-negative INTEGERs.
fn raw_to_der(raw []u8, coord_size int) []u8 {
	r := strip_zeros(raw[..coord_size])
	s := strip_zeros(raw[coord_size..coord_size * 2])
	r_int := encode_der_integer(r)
	s_int := encode_der_integer(s)
	body_len := r_int.len + s_int.len
	mut out := []u8{cap: body_len + 8}
	out << 0x30
	out << encode_der_length(body_len)
	out << r_int
	out << s_int
	return out
}

fn read_der_length(buf []u8, start int) !(int, int) {
	if start >= buf.len {
		return MalformedMessage{
			reason: 'truncated DER length'
		}
	}
	first := buf[start]
	if first < 0x80 {
		return start + 1, int(first)
	}
	n := int(first & 0x7f)
	if n == 0 || n > 4 || start + 1 + n > buf.len {
		return MalformedMessage{
			reason: 'unsupported DER long-form length (${n} bytes)'
		}
	}
	mut len := u32(0)
	for i in 0 .. n {
		len = (len << 8) | u32(buf[start + 1 + i])
	}
	return start + 1 + n, int(len)
}

fn read_der_integer(buf []u8, start int) !([]u8, int) {
	if start + 2 > buf.len || buf[start] != 0x02 {
		return MalformedMessage{
			reason: 'expected INTEGER tag 0x02 at offset ${start}'
		}
	}
	idx, len := read_der_length(buf, start + 1)!
	if idx + len > buf.len {
		return MalformedMessage{
			reason: 'INTEGER content runs past buffer'
		}
	}
	mut content_start := idx
	end := idx + len
	// DER INTEGERs are sign-extended; strip the 0x00 padding byte.
	for end > content_start + 1 && buf[content_start] == 0x00 {
		content_start++
	}
	return buf[content_start..end].clone(), idx + len
}

fn encode_der_length(n int) []u8 {
	if n < 0x80 {
		return [u8(n)]
	}
	mut bytes := []u8{cap: 4}
	mut x := n
	for x > 0 {
		bytes.prepend(u8(x & 0xff))
		x >>= 8
	}
	mut out := []u8{cap: bytes.len + 1}
	out << u8(0x80 | bytes.len)
	out << bytes
	return out
}

fn encode_der_integer(content []u8) []u8 {
	// Pad with 0x00 if the high bit is set, otherwise the value would
	// be interpreted as a negative integer.
	mut body := content.clone()
	if body.len == 0 {
		body = [u8(0x00)]
	} else if body[0] & 0x80 != 0 {
		body.prepend(u8(0x00))
	}
	mut out := []u8{cap: body.len + 4}
	out << 0x02
	out << encode_der_length(body.len)
	out << body
	return out
}

fn strip_zeros(b []u8) []u8 {
	mut i := 0
	for i < b.len - 1 && b[i] == 0 {
		i++
	}
	return b[i..]
}

fn pad_into(mut dst []u8, dst_off int, src []u8, target int) {
	if src.len >= target {
		// Source is already at or above the curve byte size; copy the
		// rightmost `target` bytes (drops the sign-extension 0x00).
		off := src.len - target
		for i in 0 .. target {
			dst[dst_off + i] = src[off + i]
		}
		return
	}
	pad := target - src.len
	for i in 0 .. pad {
		dst[dst_off + i] = 0
	}
	for i in 0 .. src.len {
		dst[dst_off + pad + i] = src[i]
	}
}

// build_ec_spki constructs a SubjectPublicKeyInfo DER for an EC public
// key with the given (x, y) coordinates. RFC 5280 §4.1.2.7 + RFC 5480
// shape:
//
//   SEQUENCE {
//     SEQUENCE { OID ecPublicKey, OID <curve> },
//     BIT STRING (0x00 || 0x04 || x || y)
//   }
fn build_ec_spki(x []u8, y []u8, curve ecdsa.Nid) []u8 {
	curve_oid := match curve {
		.prime256v1 { [u8(0x06), 0x08, 0x2A, 0x86, 0x48, 0xCE, 0x3D, 0x03, 0x01, 0x07] } // 1.2.840.10045.3.1.7
		.secp384r1 { [u8(0x06), 0x05, 0x2B, 0x81, 0x04, 0x00, 0x22] } // 1.3.132.0.34
		else { []u8{} }
	}

	ec_pub_oid := [u8(0x06), 0x07, 0x2A, 0x86, 0x48, 0xCE, 0x3D, 0x02, 0x01] // 1.2.840.10045.2.1
	mut alg := []u8{}
	alg << ec_pub_oid
	alg << curve_oid
	mut alg_seq := []u8{cap: alg.len + 4}
	alg_seq << 0x30
	alg_seq << encode_der_length(alg.len)
	alg_seq << alg
	mut point := []u8{cap: 1 + x.len + y.len}
	point << 0x04 // uncompressed point header (RFC 5480 §2.2)
	point << x
	point << y
	mut bit_string := []u8{cap: point.len + 4}
	bit_string << 0x03
	bit_string << encode_der_length(point.len + 1)
	bit_string << 0x00 // unused-bits indicator
	bit_string << point
	mut spki := []u8{cap: alg_seq.len + bit_string.len + 4}
	spki << 0x30
	spki << encode_der_length(alg_seq.len + bit_string.len)
	spki << alg_seq
	spki << bit_string
	return spki
}
