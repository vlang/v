// Copyright 2025 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
// Ported to V from Go's crypto/internal/fips140/mldsa.
module mldsa

import crypto.internal.subtle

// s. 2.3, appendix a
const q = u32(8380417) // 2^23 - 2^13 + 1
const rr = u32(2365951) // R^2 mod q (R = 2^32)
const q_neg_inv = u32(4236238847) // -q^-1 mod R (appendix a: QINV = 58728449)
const mont_one = u32(4193792) // R mod q
const mont_minus_one = u32(4186625) // (q-1)*R mod q
const n = 256
const d = 13

type FieldElement = u32
type RingElement = [256]u32
type NttElement = [256]u32

fn field_to_montgomery(a u32) !FieldElement {
	if a >= q {
		return error('unreduced field element ${a}')
	}
	return field_montgomery_mul(FieldElement(a), rr)
}

fn field_sub_to_montgomery(a u32, b u32) FieldElement {
	x := a - b + q
	return field_montgomery_mul(FieldElement(x), rr)
}

fn field_from_montgomery(a FieldElement) u32 {
	return u32(field_montgomery_reduce(u64(a)))
}

fn field_centered_mod(r FieldElement) i32 {
	x := i32(field_from_montgomery(r))
	return ct_select_leq(x, i32(q / 2), x, x - i32(q))
}

fn field_infinity_norm(r FieldElement) u32 {
	x := i32(field_from_montgomery(r))
	return u32(ct_select_leq(x, i32(q / 2), x, i32(q) - x))
}

fn field_reduce_once(a u32) FieldElement {
	if a >= q {
		return FieldElement(a - q)
	}
	return FieldElement(a)
}

fn field_add(a FieldElement, b FieldElement) FieldElement {
	return field_reduce_once(u32(a) + u32(b))
}

fn field_sub(a FieldElement, b FieldElement) FieldElement {
	return field_reduce_once(u32(a) - u32(b) + q)
}

fn field_montgomery_mul(a FieldElement, b FieldElement) FieldElement {
	x := u64(a) * u64(b)
	return field_montgomery_reduce(x)
}

// algo. 49: MontgomeryReduce
fn field_montgomery_reduce(x u64) FieldElement {
	t := u32(x) * q_neg_inv
	u_ := (x + u64(t) * u64(q)) >> 32
	return field_reduce_once(u32(u_))
}

fn field_montgomery_mul_sub(a FieldElement, b FieldElement, c FieldElement) FieldElement {
	x := u64(a) * u64(u32(b) - u32(c) + q)
	return field_montgomery_reduce(x)
}

fn field_montgomery_add_mul(a FieldElement, b FieldElement, c FieldElement, d_ FieldElement) FieldElement {
	x := u64(a) * u64(b) + u64(c) * u64(d_)
	return field_montgomery_reduce(x)
}

@[direct_array_access]
fn poly_add_ring(a RingElement, b RingElement) RingElement {
	mut s := RingElement{}
	for i in 0 .. n {
		s[i] = field_add(a[i], b[i])
	}
	return s
}

@[direct_array_access]
fn poly_add_ntt(a NttElement, b NttElement) NttElement {
	mut s := NttElement{}
	for i in 0 .. n {
		s[i] = field_add(a[i], b[i])
	}
	return s
}

@[direct_array_access]
fn poly_sub_ring(a RingElement, b RingElement) RingElement {
	mut s := RingElement{}
	for i in 0 .. n {
		s[i] = field_sub(a[i], b[i])
	}
	return s
}

@[direct_array_access]
fn poly_sub_ntt(a NttElement, b NttElement) NttElement {
	mut s := NttElement{}
	for i in 0 .. n {
		s[i] = field_sub(a[i], b[i])
	}
	return s
}

// algo. 45: MultiplyNTT
@[direct_array_access]
fn ntt_mul(a NttElement, b NttElement) NttElement {
	mut p := NttElement{}
	for i in 0 .. n {
		p[i] = field_montgomery_mul(a[i], b[i])
	}
	return p
}

@[direct_array_access]
fn coefficients_exceed_bound(w RingElement, bound u32) bool {
	for i in 0 .. n {
		if field_infinity_norm(w[i]) >= bound {
			return true
		}
	}
	return false
}

fn ct_select_leq(a i32, b i32, yes i32, no i32) i32 {
	return if subtle.constant_time_less_or_eq(int(a), int(b)) == 1 { yes } else { no }
}

fn ct_select_eq(a u32, b u32, yes u32, no u32) u32 {
	return u32(subtle.constant_time_select(subtle.constant_time_eq(int(a), int(b)), int(yes),
		int(no)))
}

fn ct_abs(x i32) u32 {
	return u32(ct_select_leq(0, x, x, -x))
}
