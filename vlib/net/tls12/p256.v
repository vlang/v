// Copyright (c) 2026 V project. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// Pure-V NIST P-256 (secp256r1) arithmetic for TLS 1.2 ECDHE key exchange
// and ECDSA signature verification/signing. Built only on math.big,
// crypto.rand and crypto.sha256; no C code.
module tls12

import crypto.rand
import math.big

// P-256 domain parameters (FIPS 186-4, section D.1.2.3).
const p256_p_hex = 'FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF'
const p256_n_hex = 'FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551'
const p256_gx_hex = '6B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C296'
const p256_gy_hex = '4FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5'

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
		return error('tls12: integer too large for 32 bytes')
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
		return error('tls12: invalid ECDH private key')
	}
	pt := c.mul(d, os2ip(peer_x), os2ip(peer_y))
	if pt.inf {
		return error('tls12: ECDH produced point at infinity')
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
		return error('tls12: trailing data in ECDSA signature')
	}
	return os2ip(rb), os2ip(sb)
}
