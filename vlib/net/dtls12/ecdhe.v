// Copyright (c) 2026 V project. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// ECDHE (NIST P-256) key exchange for DTLS 1.2 (RFC 4492 §5, RFC 5246
// ECDHE suites): pure-V curve arithmetic (same double-and-add approach as
// vlib/net/tls12/p256.v, duplicated here to keep this module file-disjoint),
// ECDSA sign/verify for ServerKeyExchange authentication, and the
// ServerKeyExchange / ClientKeyExchange / ServerHelloDone codecs.
module dtls12

import crypto.rand
import crypto.sha256
import math.big

// dtls12_group_secp256r1 is the only supported named group (RFC 4492 §5.1.1).
pub const dtls12_group_secp256r1 = u16(23)

// P-256 domain parameters (FIPS 186-4, section D.1.2.3).
const p256_p_hex = 'FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF'
const p256_n_hex = 'FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551'
const p256_gx_hex = '6B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C296'
const p256_gy_hex = '4FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5'
const p256_b_hex = '5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B'

// P256 holds the curve parameters as big integers.
struct P256 {
	p  big.Integer
	n  big.Integer
	gx big.Integer
	gy big.Integer
}

// new_p256 builds the curve parameters.
fn new_p256() P256 {
	return P256{
		p:  big.integer_from_radix(p256_p_hex, 16) or { panic(err) }
		n:  big.integer_from_radix(p256_n_hex, 16) or { panic(err) }
		gx: big.integer_from_radix(p256_gx_hex, 16) or { panic(err) }
		gy: big.integer_from_radix(p256_gy_hex, 16) or { panic(err) }
	}
}

// P256Point is an affine curve point; inf marks the point at infinity.
struct P256Point {
	x   big.Integer
	y   big.Integer
	inf bool
}

// add returns p1 + p2 on the curve (a = -3).
fn (c P256) add(p1 P256Point, p2 P256Point) P256Point {
	if p1.inf {
		return p2
	}
	if p2.inf {
		return p1
	}
	if p1.x == p2.x {
		if p1.y == p2.y {
			return c.double(p1)
		}
		return P256Point{ inf: true }
	}
	num := (p2.y - p1.y).mod_euclid(c.p)
	den := (p2.x - p1.x).mod_euclid(c.p)
	lmb := (num * den.mod_inverse(c.p) or { panic(err) }) % c.p
	x3 := (((lmb * lmb) % c.p - p1.x).mod_euclid(c.p) - p2.x).mod_euclid(c.p)
	y3 := ((lmb * (p1.x - x3).mod_euclid(c.p)) % c.p - p1.y).mod_euclid(c.p)
	return P256Point{ x: x3, y: y3 }
}

// double returns 2*p on the curve (a = -3).
fn (c P256) double(p1 P256Point) P256Point {
	if p1.inf {
		return p1
	}
	if p1.y.signum == 0 {
		return P256Point{ inf: true }
	}
	three := big.integer_from_int(3)
	two := big.integer_from_int(2)
	// lmb = (3*x^2 - 3) / (2*y)
	num := (((three * p1.x) % c.p * p1.x) % c.p - three).mod_euclid(c.p)
	den := (two * p1.y) % c.p
	lmb := (num * den.mod_inverse(c.p) or { panic(err) }) % c.p
	x3 := (((lmb * lmb) % c.p - p1.x).mod_euclid(c.p) - p1.x).mod_euclid(c.p)
	y3 := ((lmb * (p1.x - x3).mod_euclid(c.p)) % c.p - p1.y).mod_euclid(c.p)
	return P256Point{ x: x3, y: y3 }
}

// mul returns k * (px, py) via double-and-add (MSB first).
fn (c P256) mul(k big.Integer, px big.Integer, py big.Integer) P256Point {
	mut acc := P256Point{ inf: true }
	base := P256Point{ x: px, y: py }
	bits := k.bit_len()
	for i := bits - 1; i >= 0; i-- {
		acc = c.double(acc)
		if k.get_bit(u32(i)) {
			acc = c.add(acc, base)
		}
	}
	return acc
}

// mul_base returns k * G.
fn (c P256) mul_base(k big.Integer) P256Point {
	return c.mul(k, c.gx, c.gy)
}

// i2osp_32 serializes x as exactly 32 big-endian bytes.
fn i2osp_32(x big.Integer) ![]u8 {
	b, _ := x.bytes()
	if b.len > 32 {
		return error('dtls12: integer too large for 32 bytes')
	}
	mut out := []u8{len: 32}
	for i in 0 .. b.len {
		out[32 - b.len + i] = b[i]
	}
	return out
}

// os2ip parses big-endian bytes as a big integer.
fn os2ip(b []u8) big.Integer {
	return big.integer_from_bytes(b)
}

// p256_is_on_curve reports whether (x, y) satisfies the P-256 equation
// y^2 == x^3 - 3x + b (mod p). Off-curve and twist points must be
// rejected before ECDH: without this check a peer can force an
// attacker-influenced premaster (invalid-curve attack shape).
fn p256_is_on_curve(x big.Integer, y big.Integer) bool {
	c := new_p256()
	if x.signum < 0 || !(x < c.p) {
		return false
	}
	if y.signum < 0 || !(y < c.p) {
		return false
	}
	b := big.integer_from_radix(p256_b_hex, 16) or { return false }
	three := big.integer_from_int(3)
	lhs := (y * y) % c.p
	rhs := ((((x * x) % c.p * x) % c.p - (three * x) % c.p).mod_euclid(c.p) + b).mod_euclid(c.p)
	return lhs == rhs
}

// p256_keygen generates an ECDHE key pair; returns (priv, x, y) as byte strings.
fn p256_keygen() !([]u8, []u8, []u8) {
	c := new_p256()
	one := big.integer_from_int(1)
	n_minus_1 := c.n - one
	for {
		priv_raw := rand.bytes(32)!
		d := (os2ip(priv_raw) % n_minus_1) + one
		priv := i2osp_32(d)!
		pt := c.mul_base(d)
		if pt.inf {
			continue
		}
		return priv, i2osp_32(pt.x)!, i2osp_32(pt.y)!
	}
}

// p256_ecdh derives the shared secret X-coordinate (32 bytes) from our
// private key and the peer's uncompressed public point.
fn p256_ecdh(priv []u8, peer_x []u8, peer_y []u8) ![]u8 {
	c := new_p256()
	d := os2ip(priv)
	if d.signum == 0 || !(d < c.n) {
		return error('dtls12: invalid ECDH private key')
	}
	// Defense in depth: the parsers already gate this, but any direct
	// caller must still get a fail-closed off-curve rejection.
	if !p256_is_on_curve(os2ip(peer_x), os2ip(peer_y)) {
		return error('dtls12: ECDH peer point not on curve')
	}
	pt := c.mul(d, os2ip(peer_x), os2ip(peer_y))
	if pt.inf {
		return error('dtls12: ECDH produced point at infinity')
	}
	return i2osp_32(pt.x)
}

// p256_ecdsa_verify verifies a P-256 ECDSA signature (r, s) over the
// precomputed hash (truncated to 256 bits) against (qx, qy).
fn p256_ecdsa_verify(qx []u8, qy []u8, hash []u8, r big.Integer, s big.Integer) bool {
	c := new_p256()
	if r.signum == 0 || !(r < c.n) || r == c.n {
		return false
	}
	if s.signum == 0 || !(s < c.n) || s == c.n {
		return false
	}
	mut e_bytes := hash.clone()
	if e_bytes.len > 32 {
		e_bytes = e_bytes[..32]
	}
	e := os2ip(e_bytes) % c.n
	w := s.mod_inverse(c.n) or { return false }
	u1 := (e * w) % c.n
	u2 := (r * w) % c.n
	p1 := c.mul_base(u1)
	p2 := c.mul(u2, os2ip(qx), os2ip(qy))
	pt := c.add(p1, p2)
	if pt.inf {
		return false
	}
	v := (pt.x % c.n + c.n) % c.n
	return v == r
}

// p256_ecdsa_sign signs hash with private key priv (32 bytes); returns (r, s).
fn p256_ecdsa_sign(priv []u8, hash []u8) !(big.Integer, big.Integer) {
	c := new_p256()
	one := big.integer_from_int(1)
	n_minus_1 := c.n - one
	d := os2ip(priv)
	mut e_bytes := hash.clone()
	if e_bytes.len > 32 {
		e_bytes = e_bytes[..32]
	}
	e := os2ip(e_bytes) % c.n
	for {
		k_raw := rand.bytes(32)!
		k := (os2ip(k_raw) % n_minus_1) + one
		rp := c.mul_base(k)
		if rp.inf {
			continue
		}
		r := rp.x % c.n
		if r.signum == 0 {
			continue
		}
		kinv := k.mod_inverse(c.n) or { continue }
		s := (kinv * (e + r * d)) % c.n
		if s.signum == 0 {
			continue
		}
		return r, s
	}
}

// der_tlv encodes a tag-length-value triple (short/long form lengths).
fn der_tlv(tag u8, body []u8) []u8 {
	mut out := [tag]
	if body.len < 128 {
		out << u8(body.len)
	} else {
		mut lb := []u8{}
		mut n := body.len
		mut tmp := []u8{}
		for n > 0 {
			tmp << u8(n & 0xff)
			n >>= 8
		}
		for i := tmp.len - 1; i >= 0; i-- {
			lb << tmp[i]
		}
		out << u8(0x80 | lb.len)
		out << lb
	}
	out << body
	return out
}

// DerReader is a cursor over DER bytes.
struct DerReader {
	data []u8
mut:
	pos int
}

// at_end reports whether the cursor consumed all input.
fn (r &DerReader) at_end() bool {
	return r.pos >= r.data.len
}

// read_tlv reads one TLV; returns the tag number and the body bytes.
fn (mut r DerReader) read_tlv() !(int, []u8) {
	if r.pos + 2 > r.data.len {
		return error('dtls12: truncated DER TLV')
	}
	tag := int(r.data[r.pos])
	r.pos++
	mut ln := int(r.data[r.pos])
	r.pos++
	if ln & 0x80 != 0 {
		nbytes := ln & 0x7f
		if nbytes == 0 || nbytes > 4 || r.pos + nbytes > r.data.len {
			return error('dtls12: bad DER length')
		}
		ln = 0
		for _ in 0 .. nbytes {
			ln = int((u32(ln) << 8) | u32(r.data[r.pos]))
			r.pos++
		}
	}
	if ln < 0 || r.pos + ln > r.data.len {
		return error('dtls12: DER length overruns input')
	}
	body := r.data[r.pos..r.pos + ln].clone()
	r.pos += ln
	return tag, body
}

// expect reads one TLV and checks its tag.
fn (mut r DerReader) expect(tag int) ![]u8 {
	got, body := r.read_tlv()!
	if got != tag {
		return error('dtls12: unexpected DER tag ${got}, want ${tag}')
	}
	return body
}

// ecdsa_sig_to_der encodes (r, s) as a DER SEQUENCE of two INTEGERs.
fn ecdsa_sig_to_der(r big.Integer, s big.Integer) []u8 {
	rb, _ := r.bytes()
	sb, _ := s.bytes()
	mut ri := []u8{}
	if rb.len > 0 && rb[0] & 0x80 != 0 {
		ri << u8(0)
	}
	ri << rb
	mut si := []u8{}
	if sb.len > 0 && sb[0] & 0x80 != 0 {
		si << u8(0)
	}
	si << sb
	mut inner := []u8{}
	inner << der_tlv(0x02, ri)
	inner << der_tlv(0x02, si)
	return der_tlv(0x30, inner)
}

// ecdsa_sig_from_der parses a DER-encoded ECDSA signature into (r, s).
fn ecdsa_sig_from_der(data []u8) !(big.Integer, big.Integer) {
	mut r := DerReader{ data: data }
	seq := r.expect(0x30)!
	mut ri := DerReader{ data: seq }
	rb := ri.expect(0x02)!
	sb := ri.expect(0x02)!
	if !ri.at_end() {
		return error('dtls12: trailing data in ECDSA signature')
	}
	return os2ip(rb), os2ip(sb)
}

// Dtls12Ske is a parsed ServerKeyExchange message.
pub struct Dtls12Ske {
pub:
	params   []u8
	eph_x    []u8
	eph_y    []u8
	hash_alg u8
	sig_alg  u8
	sig      []u8
}

// build_server_key_exchange builds an ECDHE ServerKeyExchange body (without
// framing): named-curve params (secp256r1 + ephemeral uncompressed point)
// followed by an ECDSA-SHA256 signature over
// client_random + server_random + params, mirroring the tls12 SKE layout.
pub fn build_server_key_exchange(client_random []u8, server_random []u8, eph_x []u8, eph_y []u8, sign_priv []u8) ![]u8 {
	if client_random.len != 32 || server_random.len != 32 {
		return error('dtls12: randoms must be 32 bytes')
	}
	if eph_x.len != 32 || eph_y.len != 32 {
		return error('dtls12: bad ECDHE share')
	}
	if sign_priv.len != 32 {
		return error('dtls12: bad SKE signing key')
	}
	mut params := []u8{cap: 69}
	params << u8(3)
	put_u16(mut params, dtls12_group_secp256r1)
	params << u8(65)
	params << u8(0x04)
	params << eph_x
	params << eph_y
	mut signed := []u8{cap: 64 + params.len}
	signed << client_random
	signed << server_random
	signed << params
	hash := sha256.sum256(signed)
	r, s := p256_ecdsa_sign(sign_priv, hash)!
	sig := ecdsa_sig_to_der(r, s)
	mut body := []u8{cap: params.len + 4 + sig.len}
	body << params
	body << u8(4)
	body << u8(3)
	put_u16(mut body, u16(sig.len))
	body << sig
	return body
}

// parse_server_key_exchange parses an ECDHE ServerKeyExchange body.
pub fn parse_server_key_exchange(body []u8) !Dtls12Ske {
	if body.len < 73 {
		return error('dtls12: short ServerKeyExchange')
	}
	if body[0] != 3 {
		return error('dtls12: only named-curve ServerKeyExchange supported')
	}
	if get_u16(body[1..3]) != dtls12_group_secp256r1 {
		return error('dtls12: only secp256r1 ServerKeyExchange supported')
	}
	if body[3] != 65 {
		return error('dtls12: bad ServerKeyExchange pubkey length')
	}
	point := body[4..69]
	if point[0] != 0x04 {
		return error('dtls12: only uncompressed ServerKeyExchange points supported')
	}
	eph_x := point[1..33].clone()
	eph_y := point[33..65].clone()
	hash_alg := body[69]
	sig_alg := body[70]
	sig_len := int(get_u16(body[71..73]))
	if 73 + sig_len != body.len {
		return error('dtls12: bad ServerKeyExchange signature length')
	}
	sig := body[73..].clone()
	p := new_p256().p
	if !(os2ip(eph_x) < p) || !(os2ip(eph_y) < p) {
		return error('dtls12: ServerKeyExchange point out of range')
	}
	// Reject off-curve/twist points before ECDH (invalid-curve shape).
	if !p256_is_on_curve(os2ip(eph_x), os2ip(eph_y)) {
		return error('dtls12: ServerKeyExchange point not on curve')
	}
	return Dtls12Ske{
		params:   body[..69].clone()
		eph_x:    eph_x
		eph_y:    eph_y
		hash_alg: hash_alg
		sig_alg:  sig_alg
		sig:      sig
	}
}

// verify_server_key_exchange verifies the SKE signature over
// client_random + server_random + params against the pinned peer public key
// (65-byte uncompressed P-256 point). Only ECDSA-SHA256 is accepted.
pub fn verify_server_key_exchange(ske Dtls12Ske, client_random []u8, server_random []u8, peer_pub []u8) ! {
	if ske.hash_alg != 4 || ske.sig_alg != 3 {
		return error('dtls12: unsupported SKE signature scheme (want ECDSA-SHA256)')
	}
	if peer_pub.len != 65 || peer_pub[0] != 0x04 {
		return error('dtls12: bad pinned peer key')
	}
	mut signed := []u8{cap: 64 + ske.params.len}
	signed << client_random
	signed << server_random
	signed << ske.params
	hash := sha256.sum256(signed)
	r, s := ecdsa_sig_from_der(ske.sig)!
	if !p256_ecdsa_verify(peer_pub[1..33], peer_pub[33..65], hash, r, s) {
		return error('dtls12: bad ServerKeyExchange ECDSA signature')
	}
}

// build_client_key_exchange builds a ClientKeyExchange body (without framing):
// a length-prefixed uncompressed ephemeral point, mirroring tls12 CKE.
pub fn build_client_key_exchange(pub_x []u8, pub_y []u8) ![]u8 {
	if pub_x.len != 32 || pub_y.len != 32 {
		return error('dtls12: bad ECDHE share')
	}
	mut body := []u8{cap: 66}
	body << u8(65)
	body << u8(0x04)
	body << pub_x
	body << pub_y
	return body
}

// parse_client_key_exchange extracts the peer ephemeral point.
pub fn parse_client_key_exchange(body []u8) !([]u8, []u8) {
	if body.len != 66 || body[0] != 65 || body[1] != 0x04 {
		return error('dtls12: bad ClientKeyExchange')
	}
	px := body[2..34].clone()
	py := body[34..66].clone()
	p := new_p256().p
	if !(os2ip(px) < p) || !(os2ip(py) < p) {
		return error('dtls12: ClientKeyExchange point out of range')
	}
	// Reject off-curve/twist points before ECDH (invalid-curve shape).
	if !p256_is_on_curve(os2ip(px), os2ip(py)) {
		return error('dtls12: ClientKeyExchange point not on curve')
	}
	return px, py
}

// build_server_hello_done builds an empty ServerHelloDone handshake message.
pub fn build_server_hello_done() []u8 {
	return hs_msg(hstype_server_hello_done, []u8{})
}
