// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// Module rsa implements RSA signatures (RFC 8017): RSASSA-PKCS1-v1_5 and
// RSASSA-PSS over SHA-256, SHA-384 and SHA-512, the algorithms behind the
// RS256/RS384/RS512 and PS256/PS384/PS512 JSON Web Signatures.
module rsa

import crypto
import math.big
import crypto.subtle

// min_key_bits is the smallest modulus accepted, the minimum RFC 7518 sets for
// RSA JSON Web Signatures.
pub const min_key_bits = 2048

// max_key_bits is the largest modulus accepted.
pub const max_key_bits = 16384

// pss_salt_length_auto, as `PSSOptions.salt_length`, accepts any salt length
// when verifying, and uses the longest possible salt when signing.
pub const pss_salt_length_auto = -1

// pss_salt_length_equals_hash, as `PSSOptions.salt_length`, uses a salt as long
// as the digest. It is the default, and what PS256, PS384 and PS512 require.
pub const pss_salt_length_equals_hash = -2

// PSSOptions sets the salt length of RSASSA-PSS signatures. MGF1 always uses
// the hash of the signature.
@[params]
pub struct PSSOptions {
pub:
	salt_length int = pss_salt_length_equals_hash
}

// PublicKey is an RSA public key: a modulus and a public exponent.
pub struct PublicKey {
	n       Modulus
	n_bytes []u8
	e       int
}

// PrivateKey is an RSA private key with two prime factors. Signing uses the
// Chinese remainder theorem, with data-independent control flow and memory
// accesses; see the README for the extent of that guarantee.
pub struct PrivateKey {
	public_key PublicKey
	d          []u8
	p          Modulus
	q          Modulus
	p_bytes    []u8
	q_bytes    []u8
	dp         []u8
	dq         []u8
	qinv       []u64
	qinv_bytes []u8
}

// PrivateKeyComponents holds the numbers of an RSA private key, as big-endian
// magnitudes. `dp`, `dq` and `qinv` are computed when they are empty.
pub struct PrivateKeyComponents {
pub:
	n    []u8
	e    int
	d    []u8
	p    []u8
	q    []u8
	dp   []u8
	dq   []u8
	qinv []u8
}

// PublicKey.new returns the public key with the big-endian modulus `modulus`
// and the public exponent `exponent`.
pub fn PublicKey.new(modulus []u8, exponent int) !PublicKey {
	n := trim_zeros(modulus)
	if n.len == 0 {
		return error('crypto.rsa: empty modulus')
	}
	bit_len := (n.len - 1) * 8 + bit_length(n[0])
	if bit_len < min_key_bits || bit_len > max_key_bits {
		return error('crypto.rsa: ${bit_len}-bit modulus, expected ${min_key_bits} to ${max_key_bits} bits')
	}
	if exponent < 3 || exponent & 1 == 0 {
		return error('crypto.rsa: public exponent must be odd and at least 3')
	}
	return PublicKey{
		n:       new_modulus(n)!
		n_bytes: n
		e:       exponent
	}
}

// size returns the length of the modulus in bytes, which is also the length of
// a signature.
pub fn (k &PublicKey) size() int {
	return k.n.size()
}

// bits returns the length of the modulus in bits.
pub fn (k &PublicKey) bits() int {
	return k.n.bits
}

// modulus returns the modulus as a big-endian magnitude.
pub fn (k &PublicKey) modulus() []u8 {
	return k.n_bytes.clone()
}

// exponent returns the public exponent.
pub fn (k &PublicKey) exponent() int {
	return k.e
}

// equal reports whether `k` and `other` are the same public key.
pub fn (k &PublicKey) equal(other PublicKey) bool {
	return k.e == other.e && k.n_bytes == other.n_bytes
}

// verify_pkcs1v15 reports whether `sig` is a valid RSASSA-PKCS1-v1_5 signature
// of `digest`, the `h` digest of the message. It returns an error when `h` is
// not supported or `digest` has the wrong length.
pub fn (k &PublicKey) verify_pkcs1v15(h crypto.Hash, digest []u8, sig []u8) !bool {
	expected := emsa_pkcs1v15_encode(h, digest, k.size())!
	em := k.public_op(sig) or { return false }
	return subtle.constant_time_compare(em, expected) == 1
}

// verify_pss reports whether `sig` is a valid RSASSA-PSS signature of
// `digest`, the `h` digest of the message, with MGF1 over `h` and the salt
// length of `opts`. It returns an error when `h` is not supported or `digest`
// has the wrong length.
pub fn (k &PublicKey) verify_pss(h crypto.Hash, digest []u8, sig []u8, opts PSSOptions) !bool {
	check_digest(h, digest)!
	salt_len := match opts.salt_length {
		pss_salt_length_equals_hash { digest.len }
		pss_salt_length_auto { -1 }
		else { opts.salt_length }
	}
	if salt_len < -1 {
		return error('crypto.rsa: invalid PSS salt length ${opts.salt_length}')
	}
	mut em := k.public_op(sig) or { return false }
	em_len := (k.bits() - 1 + 7) / 8
	if em_len < em.len {
		// The encoded message has one byte fewer than the modulus when its
		// bit length is a multiple of 8 plus one; that byte must be zero.
		if em[0] != 0 {
			return false
		}
		em = em[1..].clone()
	}
	return emsa_pss_verify(h, digest, em, k.bits(), salt_len)!
}

// public_op returns `sig^e mod n` as `size()` bytes, after checking that `sig`
// is a number below the modulus of exactly `size()` bytes.
fn (k &PublicKey) public_op(sig []u8) ![]u8 {
	if sig.len != k.size() {
		return error('crypto.rsa: signature length')
	}
	s := nat_from_bytes(sig, k.n.limbs())
	if nat_less(s, k.n.n) == 0 {
		return error('crypto.rsa: signature out of range')
	}
	return nat_to_bytes(k.n.exp(s, exponent_bytes(k.e)), k.size())
}

// PrivateKey.new returns the private key made of `c`, after checking that its
// numbers are consistent.
pub fn PrivateKey.new(c PrivateKeyComponents) !PrivateKey {
	pub_key := PublicKey.new(c.n, c.e)!
	n := big.integer_from_bytes(pub_key.n_bytes)
	e := big.integer_from_int(c.e)
	d := big.integer_from_bytes(c.d)
	p := big.integer_from_bytes(c.p)
	q := big.integer_from_bytes(c.q)
	one := big.integer_from_int(1)
	if c.p.len == 0 || c.q.len == 0 {
		return error('crypto.rsa: private key needs its prime factors p and q')
	}
	if p <= one || q <= one || p == q || p * q != n {
		return error('crypto.rsa: invalid prime factors')
	}
	if d <= one || !(d < n) {
		return error('crypto.rsa: invalid private exponent')
	}
	p1 := p - one
	q1 := q - one
	dp := d % p1
	dq := d % q1
	// d must invert e modulo p - 1 and q - 1.
	if (e * dp) % p1 != one || (e * dq) % q1 != one {
		return error('crypto.rsa: private exponent does not match the public exponent')
	}
	if c.dp.len > 0 && big.integer_from_bytes(c.dp) != dp {
		return error('crypto.rsa: invalid dp')
	}
	if c.dq.len > 0 && big.integer_from_bytes(c.dq) != dq {
		return error('crypto.rsa: invalid dq')
	}
	qinv := q.mod_inverse(p) or { return error('crypto.rsa: invalid prime factors') }
	if c.qinv.len > 0 && big.integer_from_bytes(c.qinv) != qinv {
		return error('crypto.rsa: invalid qinv')
	}
	p_bytes := magnitude(p)
	q_bytes := magnitude(q)
	pm := new_modulus(p_bytes)!
	qm := new_modulus(q_bytes)!
	qinv_bytes := magnitude(qinv)
	return PrivateKey{
		public_key: pub_key
		d:          left_pad(magnitude(d), pub_key.size())
		p:          pm
		q:          qm
		p_bytes:    p_bytes
		q_bytes:    q_bytes
		dp:         left_pad(magnitude(dp), pm.size())
		dq:         left_pad(magnitude(dq), qm.size())
		qinv:       nat_from_bytes(qinv_bytes, pm.limbs())
		qinv_bytes: qinv_bytes
	}
}

// public_key returns the public half of `k`.
pub fn (k &PrivateKey) public_key() PublicKey {
	return k.public_key
}

// components returns the numbers of `k`, as big-endian magnitudes.
pub fn (k &PrivateKey) components() PrivateKeyComponents {
	return PrivateKeyComponents{
		n:    k.public_key.n_bytes.clone()
		e:    k.public_key.e
		d:    trim_zeros(k.d).clone()
		p:    k.p_bytes.clone()
		q:    k.q_bytes.clone()
		dp:   trim_zeros(k.dp).clone()
		dq:   trim_zeros(k.dq).clone()
		qinv: k.qinv_bytes.clone()
	}
}

// equal reports whether `k` and `other` are the same private key.
pub fn (k &PrivateKey) equal(other PrivateKey) bool {
	return k.public_key.equal(other.public_key)
		&& subtle.constant_time_compare(k.d, other.d) == 1
}

// sign_pkcs1v15 returns the RSASSA-PKCS1-v1_5 signature of `digest`, the `h`
// digest of the message. The signature is deterministic.
pub fn (k &PrivateKey) sign_pkcs1v15(h crypto.Hash, digest []u8) ![]u8 {
	em := emsa_pkcs1v15_encode(h, digest, k.public_key.size())!
	return k.private_op(em)
}

// sign_pss returns an RSASSA-PSS signature of `digest`, the `h` digest of the
// message, with MGF1 over `h` and a random salt of the length `opts` sets.
pub fn (k &PrivateKey) sign_pss(h crypto.Hash, digest []u8, opts PSSOptions) ![]u8 {
	h_len := hash_size(h)!
	mod_bits := k.public_key.bits()
	salt_len := match opts.salt_length {
		pss_salt_length_equals_hash { h_len }
		pss_salt_length_auto { (mod_bits - 1 + 7) / 8 - h_len - 2 }
		else { opts.salt_length }
	}
	if salt_len < 0 {
		return error('crypto.rsa: invalid PSS salt length ${opts.salt_length}')
	}
	em := emsa_pss_encode(h, digest, mod_bits, salt_len)!
	return k.private_op(em)
}

// private_op returns `em^d mod n` as a signature of `size()` bytes. `em` is an
// encoded message, which is below the modulus by construction. The result is
// checked with the public key before it is returned, so that a computation
// fault cannot leak the prime factors.
fn (k &PrivateKey) private_op(em []u8) ![]u8 {
	n := k.public_key.n
	c := nat_from_bytes(em, n.limbs())
	// m1 = c^dp mod p, m2 = c^dq mod q
	m1 := k.p.exp(k.p.reduce(c), k.dp)
	m2 := k.q.exp(k.q.reduce(c), k.dq)
	// h = qinv * (m1 - m2) mod p
	mut diff := m1.clone()
	borrow := nat_sub(mut diff, k.p.reduce(m2))
	mut wrapped := diff.clone()
	nat_add(mut wrapped, k.p.n)
	ct_select(mut diff, borrow, wrapped, diff)
	hh := k.p.mont_mul(k.p.mont_mul(diff, k.qinv), k.p.rr)
	// m = m2 + h * q, which is below n.
	product := nat_mul(hh, k.q.n)
	mut m := product[..n.limbs()].clone()
	mut m2_wide := []u64{len: n.limbs()}
	copy(mut m2_wide, m2)
	nat_add(mut m, m2_wide)
	sig := nat_to_bytes(m, k.public_key.size())
	check := k.public_key.public_op(sig) or { return error('crypto.rsa: signature check failed') }
	if subtle.constant_time_compare(check, left_pad(em, k.public_key.size())) != 1 {
		return error('crypto.rsa: signature check failed')
	}
	return sig
}

// exponent_bytes returns the big-endian bytes of a public exponent.
fn exponent_bytes(e int) []u8 {
	return [u8(e >> 24), u8(e >> 16), u8(e >> 8), u8(e)]
}

// trim_zeros returns `b` without its leading zero bytes.
fn trim_zeros(b []u8) []u8 {
	mut i := 0
	for i < b.len && b[i] == 0 {
		i++
	}
	return b[i..]
}

// left_pad returns `b` padded with leading zero bytes to `size` bytes.
fn left_pad(b []u8, size int) []u8 {
	if b.len >= size {
		return b.clone()
	}
	mut out := []u8{len: size - b.len}
	out << b
	return out
}

// bit_length returns the number of significant bits of `b`.
fn bit_length(b u8) int {
	mut n := 0
	mut x := b
	for x != 0 {
		n++
		x >>= 1
	}
	return n
}

// magnitude returns the big-endian bytes of a non-negative big.Integer.
fn magnitude(x big.Integer) []u8 {
	b, _ := x.bytes()
	return b
}
