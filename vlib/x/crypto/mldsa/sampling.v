// Copyright 2025 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
// Ported to V from Go's crypto/internal/fips140/mldsa.
module mldsa

import crypto.sha3

// algo. 30: RejNTTPoly (s. 7.3)
@[direct_array_access]
fn sample_ntt(rho []u8, s u8, r u8) NttElement {
	mut g := sha3.new_shake128()
	g.write(rho)
	g.write([s, r])

	mut a := NttElement{}
	mut j := 0
	mut buf := g.read(168)
	mut off := 0
	for j < n {
		if off + 2 >= buf.len {
			buf = g.read(168)
			off = 0
		}
		v := u32(buf[off]) | (u32(buf[off + 1]) << 8) | (u32(buf[off + 2]) << 16)
		off += 3
		candidate := v & 0x7fffff
		if candidate < q {
			a[j] = field_to_montgomery(candidate) or { continue }
			j++
		}
	}
	return a
}

// algo. 31: RejBoundedPoly (s. 7.3)
@[direct_array_access]
fn sample_bounded_poly(rho []u8, r u8, p Params) RingElement {
	mut h := sha3.new_shake256()
	h.write(rho)
	h.write([r, 0])

	mut a := RingElement{}
	mut j := 0
	mut buf := h.read(136)
	mut off := 0
	for {
		if off >= buf.len {
			buf = h.read(136)
			off = 0
		}
		z0 := buf[off] & 0x0f
		z1 := buf[off] >> 4
		off++

		coeff, ok := coeff_from_half_byte(z0, p)
		if ok {
			a[j] = coeff
			j++
		}
		if j >= n {
			break
		}

		coeff2, ok2 := coeff_from_half_byte(z1, p)
		if ok2 {
			a[j] = coeff2
			j++
		}
		if j >= n {
			break
		}
	}
	return a
}

// algo. 29: SampleInBall (s. 7.3)
@[direct_array_access]
fn sample_in_ball(rho []u8, p Params) RingElement {
	mut h := sha3.new_shake256()
	h.write(rho)
	s := h.read(8)

	// pre-read ~2x expected bytes to avoid per-byte allocations
	mut buf := h.read(p.tau * 2)
	mut off := 0

	mut c := RingElement{}
	for i := 256 - p.tau; i < 256; i++ {
		for {
			if off >= buf.len {
				buf = h.read(256)
				off = 0
			}
			j := buf[off]
			off++
			if j <= u8(i) {
				c[i] = c[j]
				bit_idx := i + p.tau - 256
				bit := (s[bit_idx / 8] >> (bit_idx % 8)) & 1
				if bit == 0 {
					c[j] = mont_one
				} else {
					c[j] = mont_minus_one
				}
				break
			}
		}
	}
	return c
}

// algo. 15: CoeffFromHalfByte (s. 7.1)
fn coeff_from_half_byte(b u8, p Params) (FieldElement, bool) {
	match p.eta {
		2 {
			if b > 14 {
				return FieldElement(0), false
			}
			quotient := (u32(b) * 0x3334) >> 16 // barrett b % 5
			remainder := u32(b) - quotient * 5
			return field_sub_to_montgomery(2, remainder), true
		}
		4 {
			if b > 8 {
				return FieldElement(0), false
			}
			return field_sub_to_montgomery(4, u32(b)), true
		}
		else {
			panic('mldsa: unsupported eta') // unreachable
		}
	}
}

// algo. 32: ExpandA (s. 7.3)
fn compute_matrix_a(rho []u8, p Params) []NttElement {
	mut a := []NttElement{len: p.k * p.l}
	for r in 0 .. p.k {
		for s in 0 .. p.l {
			a[r * p.l + s] = sample_ntt(rho, u8(s), u8(r))
		}
	}
	return a
}
