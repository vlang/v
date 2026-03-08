// Copyright 2025 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
// Ported to V from Go's crypto/internal/fips140/mldsa.

// ML-DSA (Module-Lattice-Based Digital Signature Algorithm) per FIPS 204
// https://nvlpubs.nist.gov/nistpubs/fips/nist.fips.204.pdf

module mldsa

import crypto.rand
import crypto.sha3
import crypto.internal.subtle

@[direct_array_access]
fn slice_to_32(s []u8) [32]u8 {
	mut a := [32]u8{}
	for i in 0 .. 32 {
		a[i] = s[i]
	}
	return a
}

@[direct_array_access]
fn slice_to_64(s []u8) [64]u8 {
	mut a := [64]u8{}
	for i in 0 .. 64 {
		a[i] = s[i]
	}
	return a
}

pub struct PrivateKey {
	seed [32]u8
	pk   PublicKey
	s1   []NttElement // len = l
	s2   []NttElement // len = k
	t0   []NttElement // len = k
	k    [32]u8
}

pub struct PublicKey {
	raw []u8
	p   Params
	a   []NttElement // k*l matrix in NTT domain
	t1  []NttElement // len = k, NTT(t1 * 2^d)
	tr  [64]u8
}

// algo. 1: ML-DSA.KeyGen (s. 5.1)
pub fn PrivateKey.generate(kind Kind) !PrivateKey {
	return new_private_key(slice_to_32(rand.read(32)!), kind.params())
}

pub fn PrivateKey.from_seed(seed []u8, kind Kind) !PrivateKey {
	if seed.len != 32 {
		return error('invalid seed length')
	}
	return new_private_key(slice_to_32(seed), kind.params())
}

// from FIPS 204 semi-expanded encoding. seed() and equal() are
// meaningless on the result — use from_seed when possible.
pub fn PrivateKey.from_bytes(raw []u8, kind Kind) !PrivateKey {
	return new_private_key_from_bytes(raw, kind.params())
}

pub fn PublicKey.from_bytes(raw []u8, kind Kind) !PublicKey {
	return new_public_key(raw, kind.params())
}

pub fn (sk &PrivateKey) public_key() &PublicKey {
	return &sk.pk
}

pub fn (sk &PrivateKey) seed() []u8 {
	mut s := []u8{len: 32}
	for i in 0 .. 32 {
		s[i] = sk.seed[i]
	}
	return s
}

pub fn (sk &PrivateKey) bytes() []u8 {
	return sk_encode(sk.pk.raw[..32], sk.k, sk.pk.tr, sk.s1, sk.s2, sk.t0, sk.pk.p)
}

// seed-based constant-time comparison. not meaningful for from_bytes keys.
pub fn (sk &PrivateKey) equal(other &PrivateKey) bool {
	mut a := []u8{len: 32}
	mut b := []u8{len: 32}
	for i in 0 .. 32 {
		a[i] = sk.seed[i]
		b[i] = other.seed[i]
	}
	return sk.pk.p == other.pk.p && subtle.constant_time_compare(a, b) == 1
}

// constant-time comparison of the serialized key material. slower but works for from_bytes keys.
pub fn (sk &PrivateKey) equal_bytes(other &PrivateKey) bool {
	return sk.pk.p == other.pk.p && subtle.constant_time_compare(sk.bytes(), other.bytes()) == 1
}

// algo. 2: ML-DSA.Sign (s. 5.2)
pub fn (sk &PrivateKey) sign(msg []u8, opts SignerOpts) ![]u8 {
	if opts.context.len > 255 {
		return error('context too long')
	}
	mu := compute_mu(sk.pk.tr[..], msg, opts.context)
	if opts.deterministic {
		return sign_internal(sk, mu, [32]u8{})
	}
	return sign_internal(sk, mu, slice_to_32(rand.read(32)!))
}

pub fn (pk &PublicKey) bytes() []u8 {
	return pk.raw.clone()
}

pub fn (pk &PublicKey) equal(other &PublicKey) bool {
	return pk.p == other.p && subtle.constant_time_compare(pk.raw, other.raw) == 1
}

// algo. 3: ML-DSA.Verify (s. 5.3)
pub fn (pk &PublicKey) verify(msg []u8, sig []u8, opts SignerOpts) !bool {
	if opts.context.len > 255 {
		return error('context too long')
	}
	mu := compute_mu(pk.tr[..], msg, opts.context)
	return verify_internal(pk, mu, sig)
}

pub fn (pk &PublicKey) verify_mu(mu []u8, sig []u8) !bool {
	if mu.len != 64 {
		return error('mu must be exactly 64 bytes')
	}
	return verify_internal(pk, slice_to_64(mu), sig)
}

// algo. 6: ML-DSA.KeyGen_internal (s. 6.1)
fn new_private_key(seed [32]u8, p Params) PrivateKey {
	k, l := p.k, p.l

	// expand seed into rho, rho', K
	mut xi := sha3.new_shake256()
	xi.write(seed[..])
	xi.write([u8(k), u8(l)])
	rho := xi.read(32)
	rho_s := xi.read(64)
	k_bytes := xi.read(32)

	a := compute_matrix_a(rho, p)

	mut s1 := []NttElement{len: l}
	for r in 0 .. l {
		s1[r] = ntt(sample_bounded_poly(rho_s, u8(r), p))
	}
	mut s2 := []NttElement{len: k}
	for r in 0 .. k {
		s2[r] = ntt(sample_bounded_poly(rho_s, u8(l + r), p))
	}

	// t_hat = A_hat * s1_hat + s2_hat
	mut t_hat := []NttElement{len: k}
	for i in 0 .. k {
		t_hat[i] = s2[i]
		for j in 0 .. l {
			t_hat[i] = poly_add_ntt(t_hat[i], ntt_mul(a[i * l + j], s1[j]))
		}
	}

	mut t1 := [][]u16{len: k, init: []u16{len: n}}
	mut t0 := []NttElement{len: k}
	for i in 0 .. k {
		t_i := inverse_ntt(t_hat[i])
		mut w := RingElement{}
		for j in 0 .. n {
			t1[i][j], w[j] = power2_round(t_i[j])
		}
		t0[i] = ntt(w)
	}

	pk_bytes := pk_encode(rho, t1, p)
	tr := compute_pk_hash(pk_bytes)
	t1_hat := compute_t1_hat(t1)

	k_arr := slice_to_32(k_bytes)

	return PrivateKey{
		seed: seed
		pk:   PublicKey{
			raw: pk_bytes
			p:   p
			a:   a
			t1:  t1_hat
			tr:  tr
		}
		s1:   s1
		s2:   s2
		t0:   t0
		k:    k_arr
	}
}

fn new_private_key_from_bytes(sk []u8, p Params) !PrivateKey {
	k, l := p.k, p.l

	rho, capital_k, tr, s1_ring, s2_ring, t0_ring := sk_decode(sk, p)!

	a := compute_matrix_a(rho, p)

	mut s1 := []NttElement{len: l}
	for r in 0 .. l {
		s1[r] = ntt(s1_ring[r])
	}
	mut s2 := []NttElement{len: k}
	for r in 0 .. k {
		s2[r] = ntt(s2_ring[r])
	}
	mut t0 := []NttElement{len: k}
	for r in 0 .. k {
		t0[r] = ntt(t0_ring[r])
	}

	// recompute t1 from rho, s1, s2 to verify consistency
	mut t1 := [][]u16{len: k, init: []u16{len: n}}
	for i in 0 .. k {
		mut t_hat := s2[i]
		for j in 0 .. l {
			t_hat = poly_add_ntt(t_hat, ntt_mul(a[i * l + j], s1[j]))
		}
		t_i := inverse_ntt(t_hat)
		for j in 0 .. n {
			r1, r0 := power2_round(t_i[j])
			t1[i][j] = r1
			if r0 != t0_ring[i][j] {
				return error('mldsa: private key inconsistent with t0')
			}
		}
	}

	pk_bytes := pk_encode(rho, t1, p)
	computed_tr := compute_pk_hash(pk_bytes)
	if computed_tr != tr {
		return error('mldsa: private key inconsistent with public key hash')
	}
	t1_hat := compute_t1_hat(t1)

	// use random bytes for seed since the semi-expanded format doesn't contain it
	seed := slice_to_32(rand.read(32)!)

	return PrivateKey{
		seed: seed
		pk:   PublicKey{
			raw: pk_bytes
			p:   p
			a:   a
			t1:  t1_hat
			tr:  tr
		}
		s1:   s1
		s2:   s2
		t0:   t0
		k:    capital_k
	}
}

fn new_public_key(raw []u8, p Params) !PublicKey {
	k, l := p.k, p.l

	rho, t1 := pk_decode(raw, p)!
	a := compute_matrix_a(rho, p)
	tr := compute_pk_hash(raw)
	t1_hat := compute_t1_hat(t1)

	return PublicKey{
		raw: raw.clone()
		p:   p
		a:   a[..k * l].clone()
		t1:  t1_hat[..k].clone()
		tr:  tr
	}
}

// algo. 2, lines 10-11: M' = 0x00 || |ctx| || ctx || M; mu = H(tr || M', 64)
fn compute_mu(tr []u8, msg []u8, context string) [64]u8 {
	mut h := sha3.new_shake256()
	h.write(tr)
	h.write([u8(0)]) // pure mode domain sep
	h.write([u8(context.len)])
	h.write(context.bytes())
	h.write(msg)
	return slice_to_64(h.read(64))
}

// algo. 7: ML-DSA.Sign_internal (s. 6.2)
@[direct_array_access]
fn sign_internal(sk &PrivateKey, mu [64]u8, random [32]u8) []u8 {
	p := sk.pk.p
	k, l := p.k, p.l
	a := sk.pk.a
	s1 := sk.s1
	s2 := sk.s2
	t0 := sk.t0

	beta := u32(p.tau * p.eta)
	gamma1 := u32(1) << p.gamma1
	gamma1_beta := gamma1 - beta
	gamma2 := (q - 1) / u32(p.gamma2)
	gamma2_beta := gamma2 - beta

	// line 7: rho'' = H(K || rnd || mu, 64)
	mut h_nonce := sha3.new_shake256()
	h_nonce.write(sk.k[..])
	h_nonce.write(random[..])
	h_nonce.write(mu[..])
	nonce := h_nonce.read(64)

	mut kappa := 0

	mut y := []RingElement{len: l}
	mut y_hat := []NttElement{len: l}
	mut w := []RingElement{len: k}
	mut cs1 := []RingElement{len: l}
	mut cs2 := []RingElement{len: k}
	mut z := []RingElement{len: l}
	mut ct0 := []RingElement{len: k}
	mut h := [][256]u8{len: k, init: [256]u8{}}
	mut w1_buf := []u8{len: w1_encode_len(p)}

	// lines 10-32: rejection sampling loop
	for {
		// line 11: y = ExpandMask(rho'', kappa) (algo. 34)
		for r in 0 .. l {
			counter := [u8(kappa & 0xff), u8(kappa >> 8)]
			kappa++

			mut h_y := sha3.new_shake256()
			h_y.write(nonce)
			h_y.write(counter)
			v_bytes := h_y.read((p.gamma1 + 1) * n / 8)
			y[r] = bit_unpack(v_bytes, p)
		}

		// line 12: w = NTT^-1(A_hat * NTT(y))
		for i in 0 .. l {
			y_hat[i] = ntt(y[i])
		}
		for i in 0 .. k {
			mut w_hat := NttElement{}
			for j in 0 .. l {
				w_hat = poly_add_ntt(w_hat, ntt_mul(a[i * l + j], y_hat[j]))
			}
			w[i] = inverse_ntt(w_hat)
		}

		// line 13-14: w1 = HighBits(w); c_tilde = H(mu || w1Encode(w1), lambda/4)
		mut h_ch := sha3.new_shake256()
		h_ch.write(mu[..])
		for i in 0 .. k {
			w1_encode(high_bits(w[i], p), p, mut w1_buf)
			h_ch.write(w1_buf)
		}
		ch := h_ch.read(p.lambda / 4)

		// line 15-16: c = SampleInBall(c_tilde); c_hat = NTT(c)
		c := ntt(sample_in_ball(ch, p))

		// lines 17-20: cs1 = NTT^-1(c_hat * s1_hat); z = y + cs1
		for i in 0 .. l {
			cs1[i] = inverse_ntt(ntt_mul(c, s1[i]))
		}
		for i in 0 .. k {
			cs2[i] = inverse_ntt(ntt_mul(c, s2[i]))
		}

		// line 23: ||z||_inf >= gamma1 - beta
		mut reject := false
		for i in 0 .. l {
			z[i] = poly_add_ring(y[i], cs1[i])
			if coefficients_exceed_bound(z[i], gamma1_beta) {
				reject = true
				break
			}
		}
		if reject {
			continue
		}

		// line 23: ||r0||_inf >= gamma2 - beta
		reject = false
		for i in 0 .. k {
			r0 := poly_sub_ring(w[i], cs2[i])
			if low_bits_exceed_bound(r0, gamma2_beta, p) {
				reject = true
				break
			}
		}
		if reject {
			continue
		}

		// line 25, 28: ct0 = NTT^-1(c_hat * t0_hat); ||ct0||_inf >= gamma2
		reject = false
		for i in 0 .. k {
			ct0[i] = inverse_ntt(ntt_mul(c, t0[i]))
			if coefficients_exceed_bound(ct0[i], gamma2) {
				reject = true
				break
			}
		}
		if reject {
			continue
		}

		// line 26, 28: h = MakeHint(-ct0, w - cs2 + ct0); count(h) > omega
		mut count1s := 0
		for i in 0 .. k {
			hint_result, count := make_hint(ct0[i], w[i], cs2[i], p)
			h[i] = hint_result
			count1s += count
		}
		if count1s > p.omega {
			continue
		}

		return sig_encode(ch, z, h, p) // line 33: sigEncode(c_tilde, z, h)
	}
	return []u8{}
}

// algo. 8: ML-DSA.Verify_internal (s. 6.3)
@[direct_array_access]
fn verify_internal(pk &PublicKey, mu [64]u8, sig []u8) !bool {
	p := pk.p
	k, l := p.k, p.l
	t1 := pk.t1
	a := pk.a

	beta := u32(p.tau * p.eta)
	gamma1 := u32(1) << p.gamma1
	gamma1_beta := gamma1 - beta

	ch, z, h := sig_decode(sig, p) or { return false }

	c := ntt(sample_in_ball(ch, p))

	// line 9: w'_approx = NTT^-1(A_hat * NTT(z) - NTT(c) * NTT(t1 * 2^d))
	mut z_hat := []NttElement{len: l}
	for i in 0 .. l {
		z_hat[i] = ntt(z[i])
	}
	mut w := []RingElement{len: k}
	for i in 0 .. k {
		mut w_hat := NttElement{}
		for j in 0 .. l {
			w_hat = poly_add_ntt(w_hat, ntt_mul(a[i * l + j], z_hat[j]))
		}
		w_hat = poly_sub_ntt(w_hat, ntt_mul(c, t1[i]))
		w[i] = inverse_ntt(w_hat)
	}

	// line 10: w'1 = UseHint(h, w'_approx)
	mut w1 := [][256]u8{len: k, init: [256]u8{}}
	for i in 0 .. k {
		w1[i] = use_hint(w[i], h[i], p)
	}

	// line 12: c_tilde' = H(mu || w1Encode(w'1), lambda/4)
	mut h_ch := sha3.new_shake256()
	h_ch.write(mu[..])
	mut w1_buf := []u8{len: w1_encode_len(p)}
	for i in 0 .. k {
		w1_encode(w1[i], p, mut w1_buf)
		h_ch.write(w1_buf)
	}
	computed_ch := h_ch.read(p.lambda / 4)

	// line 13: ||z||_inf < gamma1 - beta and c_tilde == c_tilde'
	for i in 0 .. l {
		if coefficients_exceed_bound(z[i], gamma1_beta) {
			return false
		}
	}

	if subtle.constant_time_compare(ch, computed_ch) != 1 {
		return false
	}

	return true
}
