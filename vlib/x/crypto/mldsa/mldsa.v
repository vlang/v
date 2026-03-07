// Copyright 2025 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
// Ported to V from Go's crypto/internal/fips140/mldsa.
module mldsa

import crypto.rand
import crypto.sha3
import crypto.internal.subtle

fn slice_to_32(s []u8) [32]u8 {
	mut a := [32]u8{}
	for i in 0 .. 32 {
		a[i] = s[i]
	}
	return a
}

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

pub fn (sk &PrivateKey) public_key() &PublicKey {
	return &sk.pk
}

pub fn (sk &PrivateKey) bytes() []u8 {
	mut s := []u8{len: 32}
	for i in 0 .. 32 {
		s[i] = sk.seed[i]
	}
	return s
}

pub fn (sk &PrivateKey) sk_bytes() []u8 {
	return sk_encode(sk.pk.raw[..32], sk.k, sk.pk.tr, sk.s1, sk.s2, sk.t0, sk.pk.p)
}

pub fn (sk &PrivateKey) equal(other &PrivateKey) bool {
	mut a := []u8{len: 32}
	mut b := []u8{len: 32}
	for i in 0 .. 32 {
		a[i] = sk.seed[i]
		b[i] = other.seed[i]
	}
	return sk.pk.p == other.pk.p && subtle.constant_time_compare(a, b) == 1
}

pub fn (pk &PublicKey) bytes() []u8 {
	return pk.raw.clone()
}

pub fn (pk &PublicKey) equal(other &PublicKey) bool {
	return pk.p == other.p && subtle.constant_time_compare(pk.raw, other.raw) == 1
}

pub fn generate_key_44() !PrivateKey {
	return generate_key(params_44)
}

pub fn generate_key_65() !PrivateKey {
	return generate_key(params_65)
}

pub fn generate_key_87() !PrivateKey {
	return generate_key(params_87)
}

fn generate_key(p Params) !PrivateKey {
	return new_private_key(slice_to_32(rand.read(32)!), p)
}

pub fn new_private_key_44(seed []u8) !PrivateKey {
	return new_private_key_from_seed(seed, params_44)
}

pub fn new_private_key_65(seed []u8) !PrivateKey {
	return new_private_key_from_seed(seed, params_65)
}

pub fn new_private_key_87(seed []u8) !PrivateKey {
	return new_private_key_from_seed(seed, params_87)
}

fn new_private_key_from_seed(seed []u8, p Params) !PrivateKey {
	if seed.len != 32 {
		return error('invalid seed length')
	}
	return new_private_key(slice_to_32(seed), p)
}

pub fn new_public_key_44(pk []u8) !PublicKey {
	return new_public_key(pk, params_44)
}

pub fn new_public_key_65(pk []u8) !PublicKey {
	return new_public_key(pk, params_65)
}

pub fn new_public_key_87(pk []u8) !PublicKey {
	return new_public_key(pk, params_87)
}

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
		s1: s1
		s2: s2
		t0: t0
		k:  k_arr
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

pub fn sign(sk &PrivateKey, msg []u8, context string) ![]u8 {
	if context.len > 255 {
		return error('context too long')
	}
	mu := compute_mu(sk.pk.tr[..], msg, context)
	return sign_internal(sk, mu, slice_to_32(rand.read(32)!))
}

pub fn sign_deterministic(sk &PrivateKey, msg []u8, context string) ![]u8 {
	if context.len > 255 {
		return error('context too long')
	}
	mu := compute_mu(sk.pk.tr[..], msg, context)
	return sign_internal(sk, mu, [32]u8{})
}

fn compute_mu(tr []u8, msg []u8, context string) [64]u8 {
	mut h := sha3.new_shake256()
	h.write(tr)
	h.write([u8(0)]) // pure mode domain sep
	h.write([u8(context.len)])
	h.write(context.bytes())
	h.write(msg)
	return slice_to_64(h.read(64))
}

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

	mut h_nonce := sha3.new_shake256()
	h_nonce.write(sk.k[..])
	h_nonce.write(random[..])
	h_nonce.write(mu[..])
	nonce := h_nonce.read(64)

	mut kappa := 0

	// rejection sampling loop
	for {
		mut y := []RingElement{len: l}
		for r in 0 .. l {
			counter := [u8(kappa & 0xff), u8(kappa >> 8)]
			kappa++

			mut h_y := sha3.new_shake256()
			h_y.write(nonce)
			h_y.write(counter)
			v_bytes := h_y.read((p.gamma1 + 1) * n / 8)
			y[r] = bit_unpack(v_bytes, p)
		}

		// w = INTT(A_hat * NTT(y))
		mut y_hat := []NttElement{len: l}
		for i in 0 .. l {
			y_hat[i] = ntt(y[i])
		}
		mut w := []RingElement{len: k}
		for i in 0 .. k {
			mut w_hat := NttElement{}
			for j in 0 .. l {
				w_hat = poly_add_ntt(w_hat, ntt_mul(a[i * l + j], y_hat[j]))
			}
			w[i] = inverse_ntt(w_hat)
		}

		mut h_ch := sha3.new_shake256()
		h_ch.write(mu[..])
		for i in 0 .. k {
			h_ch.write(w1_encode(high_bits(w[i], p), p))
		}
		ch := h_ch.read(p.lambda / 4)

		c := ntt(sample_in_ball(ch, p))

		// z = y + c*s1
		mut cs1 := []RingElement{len: l}
		for i in 0 .. l {
			cs1[i] = inverse_ntt(ntt_mul(c, s1[i]))
		}
		mut cs2 := []RingElement{len: k}
		for i in 0 .. k {
			cs2[i] = inverse_ntt(ntt_mul(c, s2[i]))
		}

		mut z := []RingElement{len: l}
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

		mut ct0 := []RingElement{len: k}
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

		mut count1s := 0
		mut h := [][256]u8{len: k, init: [256]u8{}}
		for i in 0 .. k {
			hint_result, count := make_hint(ct0[i], w[i], cs2[i], p)
			h[i] = hint_result
			count1s += count
		}
		if count1s > p.omega {
			continue
		}

		return sig_encode(ch, z, h, p)
	}
	return []u8{}
}

pub fn verify(pk &PublicKey, msg []u8, sig []u8, context string) !bool {
	if context.len > 255 {
		return error('context too long')
	}
	mu := compute_mu(pk.tr[..], msg, context)
	return verify_internal(pk, mu, sig)
}

pub fn verify_with_mu(pk &PublicKey, mu []u8, sig []u8) !bool {
	if mu.len != 64 {
		return error('mu must be exactly 64 bytes')
	}
	return verify_internal(pk, slice_to_64(mu), sig)
}

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

	// w = A_hat * NTT(z) - NTT(c) * NTT(t1 * 2^d)
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

	mut w1 := [][256]u8{len: k, init: [256]u8{}}
	for i in 0 .. k {
		w1[i] = use_hint(w[i], h[i], p)
	}

	mut h_ch := sha3.new_shake256()
	h_ch.write(mu[..])
	for i in 0 .. k {
		h_ch.write(w1_encode(w1[i], p))
	}
	computed_ch := h_ch.read(p.lambda / 4)

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
