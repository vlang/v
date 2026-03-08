// Copyright 2025 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
// Ported to V from Go's crypto/internal/fips140/mldsa.
module mldsa

import crypto.internal.subtle
import crypto.sha3

// algo. 22: pkEncode (s. 7.2)
@[direct_array_access]
fn pk_encode(rho []u8, t1 [][]u16, p Params) []u8 {
	mut pk := rho.clone()
	for i in 0 .. p.k {
		w := t1[i]
		mut j := 0
		for j < n {
			c0 := w[j]
			c1 := w[j + 1]
			c2 := w[j + 2]
			c3 := w[j + 3]
			pk << u8(c0)
			pk << u8((c0 >> 8) | (c1 << 2))
			pk << u8((c1 >> 6) | (c2 << 4))
			pk << u8((c2 >> 4) | (c3 << 6))
			pk << u8(c3 >> 2)
			j += 4
		}
	}
	return pk
}

// algo. 23: pkDecode (s. 7.2)
@[direct_array_access]
fn pk_decode(pk []u8, p Params) !([]u8, [][]u16) {
	expected := pub_key_size(p)
	if pk.len != expected {
		return error('invalid public key length')
	}
	rho := pk[..32].clone()
	// avoid cloning on each iteration
	mut data := unsafe { pk[32..] }
	mut t1 := [][]u16{len: p.k, init: []u16{len: n}}
	for r in 0 .. p.k {
		mut j := 0
		for j < n {
			b0 := data[0]
			b1 := data[1]
			b2 := data[2]
			b3 := data[3]
			b4 := data[4]
			t1[r][j] = u16(b0) | (u16(b1 & 0x03) << 8)
			t1[r][j + 1] = u16(b1 >> 2) | (u16(b2 & 0x0f) << 6)
			t1[r][j + 2] = u16(b2 >> 4) | (u16(b3 & 0x3f) << 4)
			t1[r][j + 3] = u16(b3 >> 6) | (u16(b4) << 2)
			data = unsafe { data[5..] }
			j += 4
		}
	}
	return rho, t1
}

fn compute_pk_hash(pk []u8) [64]u8 {
	return slice_to_64(sha3.shake256(pk, 64))
}

@[direct_array_access]
fn compute_t1_hat(t1 [][]u16) []NttElement {
	mut result := []NttElement{len: t1.len}
	for i in 0 .. t1.len {
		mut w := RingElement{}
		for j in 0 .. n {
			z := field_to_montgomery(u32(t1[i][j]) << d) or { panic(err) }
			w[j] = z
		}
		result[i] = ntt(w)
	}
	return result
}

// algo. 35: Power2Round (s. 7.4)
fn power2_round(r FieldElement) (u16, FieldElement) {
	rr_ := field_from_montgomery(r)
	r1 := (rr_ + (1 << 12) - 1) >> d
	r0 := field_sub_to_montgomery(rr_, r1 << d)
	return u16(r1), r0
}

// algo. 37: HighBits (s. 7.4)
@[direct_array_access]
fn high_bits(r RingElement, p Params) [256]u8 {
	mut w := [256]u8{}
	match p.gamma2 {
		32 {
			for i in 0 .. n {
				w[i] = high_bits_32(field_from_montgomery(r[i]))
			}
		}
		88 {
			for i in 0 .. n {
				w[i] = high_bits_88(field_from_montgomery(r[i]))
			}
		}
		else {
			panic('mldsa: unsupported gamma2')
		}
	}
	return w
}

fn high_bits_32(x u32) u8 {
	mut r1 := (x + 127) >> 7 // approx div by 2*gamma2
	r1 = (r1 * 1025 + (1 << 21)) >> 22
	r1 &= 0xf
	return u8(r1)
}

fn high_bits_88(x u32) u8 {
	mut r1 := (x + 127) >> 7 // approx div by 2*gamma2
	r1 = (r1 * 11275 + (1 << 23)) >> 24
	r1 = ct_select_eq(r1, 44, 0, r1)
	return u8(r1)
}

// algo. 36: Decompose, gamma2 = (q-1)/32 (s. 7.4)
fn decompose_32(r FieldElement) (u8, i32) {
	x := field_from_montgomery(r)
	r1 := high_bits_32(x)
	r0 := i32(x) - i32(r1) * 2 * i32((q - 1) / 32)
	r0_adj := ct_select_leq(i32(q / 2 + 1), r0, r0 - i32(q), r0)
	return r1, r0_adj
}

// algo. 36: Decompose, gamma2 = (q-1)/88 (s. 7.4)
fn decompose_88(r FieldElement) (u8, i32) {
	x := field_from_montgomery(r)
	r1 := high_bits_88(x)
	r0 := i32(x) - i32(r1) * 2 * i32((q - 1) / 88)
	r0_adj := ct_select_leq(i32(q / 2 + 1), r0, r0 - i32(q), r0)
	return r1, r0_adj
}

@[direct_array_access]
fn low_bits_exceed_bound(w RingElement, bound u32, p Params) bool {
	match p.gamma2 {
		32 {
			for i in 0 .. n {
				_, r0 := decompose_32(w[i])
				if ct_abs(r0) >= bound {
					return true
				}
			}
		}
		88 {
			for i in 0 .. n {
				_, r0 := decompose_88(w[i])
				if ct_abs(r0) >= bound {
					return true
				}
			}
		}
		else {
			panic('mldsa: unsupported gamma2')
		}
	}
	return false
}

// algo. 40: UseHint (s. 7.4)
@[direct_array_access]
fn use_hint(r RingElement, h [256]u8, p Params) [256]u8 {
	mut w := [256]u8{}
	match p.gamma2 {
		32 {
			for i in 0 .. n {
				w[i] = use_hint_32(r[i], h[i])
			}
		}
		88 {
			for i in 0 .. n {
				w[i] = use_hint_88(r[i], h[i])
			}
		}
		else {
			panic('mldsa: unsupported gamma2')
		}
	}
	return w
}

fn use_hint_32(r FieldElement, hint u8) u8 {
	mut r1, r0 := decompose_32(r)
	if hint == 1 {
		if r0 > 0 {
			r1 = (r1 + 1) % 16
		} else {
			r1 = (r1 - 1) % 16
		}
	}
	return r1
}

fn use_hint_88(r FieldElement, hint u8) u8 {
	mut r1, r0 := decompose_88(r)
	if hint == 1 {
		if r0 > 0 {
			if r1 == 43 {
				r1 = 0
			} else {
				r1++
			}
		} else {
			if r1 == 0 {
				r1 = 43
			} else {
				r1--
			}
		}
	}
	return r1
}

// algo. 39: MakeHint (s. 7.4)
@[direct_array_access]
fn make_hint(ct0 RingElement, w RingElement, cs2 RingElement, p Params) ([256]u8, int) {
	mut h := [256]u8{}
	mut count := 0
	match p.gamma2 {
		32 {
			for i in 0 .. n {
				h[i] = make_hint_32(ct0[i], w[i], cs2[i])
				count += int(h[i])
			}
		}
		88 {
			for i in 0 .. n {
				h[i] = make_hint_88(ct0[i], w[i], cs2[i])
				count += int(h[i])
			}
		}
		else {
			panic('mldsa: unsupported gamma2')
		}
	}
	return h, count
}

fn make_hint_32(ct0 FieldElement, w FieldElement, cs2 FieldElement) u8 {
	r_plus_z := field_sub(w, cs2)
	v1 := high_bits_32(field_from_montgomery(r_plus_z))
	r1 := high_bits_32(field_from_montgomery(field_add(r_plus_z, ct0)))
	return u8(1 - subtle.constant_time_byte_eq(v1, r1))
}

fn make_hint_88(ct0 FieldElement, w FieldElement, cs2 FieldElement) u8 {
	r_plus_z := field_sub(w, cs2)
	v1 := high_bits_88(field_from_montgomery(r_plus_z))
	r1 := high_bits_88(field_from_montgomery(field_add(r_plus_z, ct0)))
	return u8(1 - subtle.constant_time_byte_eq(v1, r1))
}

fn w1_encode_len(p Params) int {
	return match p.gamma2 {
		32 { 4 * n / 8 }
		88 { 6 * n / 8 }
		else { panic('mldsa: unsupported gamma2') }
	}
}

// algo. 28: w1Encode (s. 7.2)
@[direct_array_access]
fn w1_encode(w [256]u8, p Params, mut buf []u8) {
	match p.gamma2 {
		32 {
			for i := 0; i < n; i += 2 {
				buf[i / 2] = w[i] | (w[i + 1] << 4)
			}
		}
		88 {
			for i := 0; i < n; i += 4 {
				buf[3 * i / 4] = w[i] | (w[i + 1] << 6)
				buf[3 * i / 4 + 1] = (w[i + 1] >> 2) | (w[i + 2] << 4)
				buf[3 * i / 4 + 2] = (w[i + 2] >> 4) | (w[i + 3] << 2)
			}
		}
		else {
			panic('mldsa: unsupported gamma2')
		}
	}
}

// algo. 26: sigEncode (s. 7.2)
fn sig_encode(ch []u8, z []RingElement, h [][256]u8, p Params) []u8 {
	mut sig := ch.clone()
	for i in 0 .. z.len {
		sig << bit_pack(z[i], p)
	}
	sig << hint_encode(h, p)
	return sig
}

// algo. 27: sigDecode (s. 7.2)
@[direct_array_access]
fn sig_decode(sig []u8, p Params) !([]u8, []RingElement, [][256]u8) {
	expected := sig_size(p)
	if sig.len != expected {
		return error('invalid signature length')
	}
	ch_len := p.lambda / 4
	ch := sig[..ch_len].clone()
	mut offset := ch_len
	mut z := []RingElement{len: p.l}
	for i in 0 .. p.l {
		length := (p.gamma1 + 1) * n / 8
		z[i] = bit_unpack(sig[offset..offset + length], p)
		offset += length
	}
	h := hint_decode(sig[offset..], p)!
	return ch, z, h
}

// algo. 17: BitPack (s. 7.1)
fn bit_pack(r RingElement, p Params) []u8 {
	match p.gamma1 {
		17 { return bit_pack_18(r) }
		19 { return bit_pack_20(r) }
		else { panic('mldsa: unsupported gamma1') }
	}
}

@[direct_array_access]
fn bit_pack_18(r RingElement) []u8 {
	mut v := []u8{len: 18 * n / 8}
	mut pos := 0
	for i := 0; i < n; i += 4 {
		w0 := u32(1 << 17) - u32(field_centered_mod(r[i]))
		w1 := u32(1 << 17) - u32(field_centered_mod(r[i + 1]))
		w2 := u32(1 << 17) - u32(field_centered_mod(r[i + 2]))
		w3 := u32(1 << 17) - u32(field_centered_mod(r[i + 3]))
		v[pos] = u8(w0)
		v[pos + 1] = u8(w0 >> 8)
		v[pos + 2] = u8(w0 >> 16)
		v[pos + 2] |= u8(w1 << 2)
		v[pos + 3] = u8(w1 >> 6)
		v[pos + 4] = u8(w1 >> 14)
		v[pos + 4] |= u8(w2 << 4)
		v[pos + 5] = u8(w2 >> 4)
		v[pos + 6] = u8(w2 >> 12)
		v[pos + 6] |= u8(w3 << 6)
		v[pos + 7] = u8(w3 >> 2)
		v[pos + 8] = u8(w3 >> 10)
		pos += 9
	}
	return v
}

@[direct_array_access]
fn bit_pack_20(r RingElement) []u8 {
	mut v := []u8{len: 20 * n / 8}
	mut pos := 0
	for i := 0; i < n; i += 2 {
		w0 := u32(1 << 19) - u32(field_centered_mod(r[i]))
		w1 := u32(1 << 19) - u32(field_centered_mod(r[i + 1]))
		v[pos] = u8(w0)
		v[pos + 1] = u8(w0 >> 8)
		v[pos + 2] = u8(w0 >> 16)
		v[pos + 2] |= u8(w1 << 4)
		v[pos + 3] = u8(w1 >> 4)
		v[pos + 4] = u8(w1 >> 12)
		pos += 5
	}
	return v
}

// algo. 19: BitUnpack (s. 7.1)
fn bit_unpack(v []u8, p Params) RingElement {
	match p.gamma1 {
		17 { return bit_unpack_18(v) }
		19 { return bit_unpack_20(v) }
		else { panic('mldsa: unsupported gamma1') }
	}
}

@[direct_array_access]
fn bit_unpack_18(v []u8) RingElement {
	mut r := RingElement{}
	mut pos := 0
	for i := 0; i < n; i += 4 {
		w0 := u32(v[pos]) | (u32(v[pos + 1]) << 8) | (u32(v[pos + 2]) << 16)
		r[i] = field_sub_to_montgomery(u32(1 << 17), w0 & 0x3ffff)
		w1 := (u32(v[pos + 2]) >> 2) | (u32(v[pos + 3]) << 6) | (u32(v[pos + 4]) << 14)
		r[i + 1] = field_sub_to_montgomery(u32(1 << 17), w1 & 0x3ffff)
		w2 := (u32(v[pos + 4]) >> 4) | (u32(v[pos + 5]) << 4) | (u32(v[pos + 6]) << 12)
		r[i + 2] = field_sub_to_montgomery(u32(1 << 17), w2 & 0x3ffff)
		w3 := (u32(v[pos + 6]) >> 6) | (u32(v[pos + 7]) << 2) | (u32(v[pos + 8]) << 10)
		r[i + 3] = field_sub_to_montgomery(u32(1 << 17), w3 & 0x3ffff)
		pos += 9
	}
	return r
}

@[direct_array_access]
fn bit_unpack_20(v []u8) RingElement {
	mut r := RingElement{}
	mut pos := 0
	for i := 0; i < n; i += 2 {
		w0 := u32(v[pos]) | (u32(v[pos + 1]) << 8) | (u32(v[pos + 2]) << 16)
		r[i] = field_sub_to_montgomery(u32(1 << 19), w0 & 0xfffff)
		w1 := (u32(v[pos + 2]) >> 4) | (u32(v[pos + 3]) << 4) | (u32(v[pos + 4]) << 12)
		r[i + 1] = field_sub_to_montgomery(u32(1 << 19), w1 & 0xfffff)
		pos += 5
	}
	return r
}

// algo. 20: HintBitPack (s. 7.2)
@[direct_array_access]
fn hint_encode(h [][256]u8, p Params) []u8 {
	mut out := []u8{len: p.omega + p.k}
	mut idx := 0
	for i in 0 .. p.k {
		for j in 0 .. n {
			if h[i][j] != 0 {
				out[idx] = u8(j)
				idx++
			}
		}
		out[p.omega + i] = u8(idx)
	}
	return out
}

// algo. 21: HintBitUnpack (s. 7.2)
@[direct_array_access]
fn hint_decode(y []u8, p Params) ![][256]u8 {
	if y.len != p.omega + p.k {
		return error('invalid hint length')
	}
	mut h := [][256]u8{len: p.k, init: [256]u8{}}
	mut idx := u8(0)
	for i in 0 .. p.k {
		limit := y[p.omega + i]
		if limit < idx || limit > u8(p.omega) {
			return error('invalid hint limits')
		}
		first := idx
		for idx < limit {
			if idx > first && y[idx - 1] >= y[idx] {
				return error('invalid hint index order')
			}
			h[i][y[idx]] = 1
			idx++
		}
	}
	for i := int(idx); i < p.omega; i++ {
		if y[i] != 0 {
			return error('invalid hint padding')
		}
	}
	return h
}

@[direct_array_access]
fn bit_pack_slow(r RingElement, a int, b int) []u8 {
	bitlen := bits_len(u32(a + b))
	mut out := []u8{len: n * bitlen / 8}
	mut acc := u32(0)
	mut acc_bits := u32(0)
	mut vi := 0
	for i in 0 .. n {
		w := u32(int(b) - int(field_centered_mod(r[i])))
		acc |= w << acc_bits
		acc_bits += u32(bitlen)
		for acc_bits >= 8 {
			out[vi] = u8(acc)
			vi++
			acc >>= 8
			acc_bits -= 8
		}
	}
	if acc_bits > 0 {
		out[vi] = u8(acc)
	}
	return out
}

@[direct_array_access]
fn bit_unpack_slow(v []u8, a int, b int) !RingElement {
	bitlen := bits_len(u32(a + b))
	if v.len != n * bitlen / 8 {
		return error('mldsa: invalid input length for bit_unpack_slow')
	}

	mask := u32((1 << bitlen) - 1)
	max_value := u32(a + b)

	mut r := RingElement{}
	mut acc := u32(0)
	mut acc_bits := u32(0)
	mut vi := 0

	for i in 0 .. n {
		for acc_bits < u32(bitlen) {
			if vi < v.len {
				acc |= u32(v[vi]) << acc_bits
				vi++
				acc_bits += 8
			}
		}
		w := acc & mask
		if w > max_value {
			return error('mldsa: coefficient out of range')
		}
		r[i] = field_sub_to_montgomery(u32(b), w)
		acc >>= u32(bitlen)
		acc_bits -= u32(bitlen)
	}

	return r
}

fn bits_len(x u32) int {
	if x == 0 {
		return 0
	}
	mut v := x
	mut n_ := 0
	for v > 0 {
		v >>= 1
		n_++
	}
	return n_
}

// algo. 24: skEncode (s. 7.2)
fn sk_encode(rho []u8, capital_k [32]u8, tr [64]u8, s1 []NttElement, s2 []NttElement, t0 []NttElement, p Params) []u8 {
	mut out := []u8{}
	out << rho[..32]
	out << capital_k[..]
	out << tr[..]
	eta := int(p.eta)
	for i in 0 .. p.l {
		out << bit_pack_slow(inverse_ntt(s1[i]), eta, eta)
	}
	for i in 0 .. p.k {
		out << bit_pack_slow(inverse_ntt(s2[i]), eta, eta)
	}
	for i in 0 .. p.k {
		out << bit_pack_slow(inverse_ntt(t0[i]), 4095, 4096)
	}
	return out
}

// algo. 25: skDecode (s. 7.2)
fn sk_decode(sk []u8, p Params) !([]u8, [32]u8, [64]u8, []RingElement, []RingElement, []RingElement) {
	k, l, eta := p.k, p.l, p.eta
	if sk.len != priv_key_size(p) {
		return error('mldsa: invalid private key size')
	}
	rho := sk[..32].clone()
	capital_k := slice_to_32(sk[32..64])
	tr := slice_to_64(sk[64..128])
	mut offset := 128

	eta_len := n * bits_len(u32(eta * 2)) / 8
	mut s1 := []RingElement{len: l}
	for i in 0 .. l {
		s1[i] = bit_unpack_slow(sk[offset..offset + eta_len], eta, eta)!
		offset += eta_len
	}

	mut s2 := []RingElement{len: k}
	for i in 0 .. k {
		s2[i] = bit_unpack_slow(sk[offset..offset + eta_len], eta, eta)!
		offset += eta_len
	}

	t0_len := n * 13 / 8
	mut t0 := []RingElement{len: k}
	for i in 0 .. k {
		t0[i] = bit_unpack_slow(sk[offset..offset + t0_len], 4095, 4096)!
		offset += t0_len
	}

	return rho, capital_k, tr, s1, s2, t0
}
