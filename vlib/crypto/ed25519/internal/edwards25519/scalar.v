module edwards25519

import rand
import encoding.binary
import crypto.internal.subtle

// A Scalar is an integer modulo
//
//     l = 2^252 + 27742317777372353535851937790883648493
//
// which is the prime order of the edwards25519 group.
//
// This type works similarly to math/big.Int, and all arguments and
// receivers are allowed to alias.
//
// The zero value is a valid zero element.
struct Scalar {
mut:
	// s is the Scalar value in little-endian. The value is always reduced
	// between operations.
	s [32]u8
}

pub const (
	sc_zero = Scalar{
		s: [u8(0), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0]!
	}

	sc_one = Scalar{
		s: [u8(1), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0]!
	}

	sc_minus_one = Scalar{
		s: [u8(236), 211, 245, 92, 26, 99, 18, 88, 214, 156, 247, 162, 222, 249, 222, 20, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16]!
	}
)

// new_scalar return new zero scalar
pub fn new_scalar() Scalar {
	return Scalar{}
}

// add sets s = x + y mod l, and returns s.
pub fn (mut s Scalar) add(x Scalar, y Scalar) Scalar {
	// s = 1 * x + y mod l
	sc_mul_add(mut s.s, edwards25519.sc_one.s, x.s, y.s)
	return s
}

// multiply_add sets s = x * y + z mod l, and returns s.
pub fn (mut s Scalar) multiply_add(x Scalar, y Scalar, z Scalar) Scalar {
	sc_mul_add(mut s.s, x.s, y.s, z.s)
	return s
}

// subtract sets s = x - y mod l, and returns s.
pub fn (mut s Scalar) subtract(x Scalar, y Scalar) Scalar {
	// s = -1 * y + x mod l
	sc_mul_add(mut s.s, edwards25519.sc_minus_one.s, y.s, x.s)
	return s
}

// negate sets s = -x mod l, and returns s.
pub fn (mut s Scalar) negate(x Scalar) Scalar {
	// s = -1 * x + 0 mod l
	sc_mul_add(mut s.s, edwards25519.sc_minus_one.s, x.s, edwards25519.sc_zero.s)
	return s
}

// multiply sets s = x * y mod l, and returns s.
pub fn (mut s Scalar) multiply(x Scalar, y Scalar) Scalar {
	// s = x * y + 0 mod l
	sc_mul_add(mut s.s, x.s, y.s, edwards25519.sc_zero.s)
	return s
}

// set sets s = x, and returns s.
pub fn (mut s Scalar) set(x Scalar) Scalar {
	s = x
	return s
}

// set_uniform_bytes sets s to an uniformly distributed value given 64 uniformly
// distributed random bytes. If x is not of the right length, set_uniform_bytes
// returns an error, and the receiver is unchanged.
pub fn (mut s Scalar) set_uniform_bytes(x []u8) !Scalar {
	if x.len != 64 {
		return error('edwards25519: invalid set_uniform_bytes input length')
	}
	mut wide_bytes := []u8{len: 64}
	copy(mut wide_bytes, x)
	// for i, item in x {
	//	wide_bytes[i] = item
	//}
	sc_reduce(mut s.s, mut wide_bytes)
	return s
}

// set_canonical_bytes sets s = x, where x is a 32-byte little-endian encoding of
// s, and returns s. If x is not a canonical encoding of s, set_canonical_bytes
// returns an error, and the receiver is unchanged.
pub fn (mut s Scalar) set_canonical_bytes(x []u8) !Scalar {
	if x.len != 32 {
		return error('invalid scalar length')
	}
	// mut bb := []u8{len:32}
	mut ss := Scalar{}
	for i, item in x {
		ss.s[i] = item
	}

	//_ := copy(mut ss.s[..], x) //its not working
	if !is_reduced(ss) {
		return error('invalid scalar encoding')
	}
	s.s = ss.s
	return s
}

// is_reduced returns whether the given scalar is reduced modulo l.
fn is_reduced(s Scalar) bool {
	for i := s.s.len - 1; i >= 0; i-- {
		if s.s[i] > edwards25519.sc_minus_one.s[i] {
			return false
		}
		if s.s[i] < edwards25519.sc_minus_one.s[i] {
			return true
		}
		/*
		switch {
		case s.s[i] > sc_minus_one.s[i]:
			return false
		case s.s[i] < sc_minus_one.s[i]:
			return true
		}
		*/
	}
	return true
}

// set_bytes_with_clamping applies the buffer pruning described in RFC 8032,
// Section 5.1.5 (also known as clamping) and sets s to the result. The input
// must be 32 bytes, and it is not modified. If x is not of the right length,
// `set_bytes_with_clamping` returns an error, and the receiver is unchanged.
//
// Note that since Scalar values are always reduced modulo the prime order of
// the curve, the resulting value will not preserve any of the cofactor-clearing
// properties that clamping is meant to provide. It will however work as
// expected as long as it is applied to points on the prime order subgroup, like
// in Ed25519. In fact, it is lost to history why RFC 8032 adopted the
// irrelevant RFC 7748 clamping, but it is now required for compatibility.
pub fn (mut s Scalar) set_bytes_with_clamping(x []u8) !Scalar {
	// The description above omits the purpose of the high bits of the clamping
	// for brevity, but those are also lost to reductions, and are also
	// irrelevant to edwards25519 as they protect against a specific
	// implementation bug that was once observed in a generic Montgomery ladder.
	if x.len != 32 {
		return error('edwards25519: invalid set_bytes_with_clamping input length')
	}

	mut wide_bytes := []u8{len: 64, cap: 64}
	copy(mut wide_bytes, x)
	// for i, item in x {
	//	wide_bytes[i] = item
	//}
	wide_bytes[0] &= 248
	wide_bytes[31] &= 63
	wide_bytes[31] |= 64
	sc_reduce(mut s.s, mut wide_bytes)
	return s
}

// bytes returns the canonical 32-byte little-endian encoding of s.
pub fn (mut s Scalar) bytes() []u8 {
	mut buf := []u8{len: 32}
	copy(mut buf, s.s[..])
	return buf
}

// equal returns 1 if s and t are equal, and 0 otherwise.
pub fn (s Scalar) equal(t Scalar) int {
	return subtle.constant_time_compare(s.s[..], t.s[..])
}

// sc_mul_add and sc_reduce are ported from the public domain, “ref10”
// implementation of ed25519 from SUPERCOP.
fn load3(inp []u8) i64 {
	mut r := i64(inp[0])
	r |= i64(inp[1]) * 256 // << 8
	r |= i64(inp[2]) * 65536 // << 16
	return r
}

fn load4(inp []u8) i64 {
	mut r := i64(inp[0])
	r |= i64(inp[1]) * 256
	r |= i64(inp[2]) * 65536
	r |= i64(inp[3]) * 16777216
	return r
}

// Input:
//   a[0]+256*a[1]+...+256^31*a[31] = a
//   b[0]+256*b[1]+...+256^31*b[31] = b
//   c[0]+256*c[1]+...+256^31*c[31] = c
//
// Output:
//   s[0]+256*s[1]+...+256^31*s[31] = (ab+c) mod l
//   where l = 2^252 + 27742317777372353535851937790883648493.
fn sc_mul_add(mut s [32]u8, a [32]u8, b [32]u8, c [32]u8) {
	a0 := 2097151 & load3(a[..])
	a1 := 2097151 & (load4(a[2..]) >> 5)
	a2 := 2097151 & (load3(a[5..]) >> 2)
	a3 := 2097151 & (load4(a[7..]) >> 7)
	a4 := 2097151 & (load4(a[10..]) >> 4)
	a5 := 2097151 & (load3(a[13..]) >> 1)
	a6 := 2097151 & (load4(a[15..]) >> 6)
	a7 := 2097151 & (load3(a[18..]) >> 3)
	a8 := 2097151 & load3(a[21..])
	a9 := 2097151 & (load4(a[23..]) >> 5)
	a10 := 2097151 & (load3(a[26..]) >> 2)
	a11 := (load4(a[28..]) >> 7)
	b0 := 2097151 & load3(b[..])
	b1 := 2097151 & (load4(b[2..]) >> 5)
	b2 := 2097151 & (load3(b[5..]) >> 2)
	b3 := 2097151 & (load4(b[7..]) >> 7)
	b4 := 2097151 & (load4(b[10..]) >> 4)
	b5 := 2097151 & (load3(b[13..]) >> 1)
	b6 := 2097151 & (load4(b[15..]) >> 6)
	b7 := 2097151 & (load3(b[18..]) >> 3)
	b8 := 2097151 & load3(b[21..])
	b9 := 2097151 & (load4(b[23..]) >> 5)
	b10 := 2097151 & (load3(b[26..]) >> 2)
	b11 := (load4(b[28..]) >> 7)
	c0 := 2097151 & load3(c[..])
	c1 := 2097151 & (load4(c[2..]) >> 5)
	c2 := 2097151 & (load3(c[5..]) >> 2)
	c3 := 2097151 & (load4(c[7..]) >> 7)
	c4 := 2097151 & (load4(c[10..]) >> 4)
	c5 := 2097151 & (load3(c[13..]) >> 1)
	c6 := 2097151 & (load4(c[15..]) >> 6)
	c7 := 2097151 & (load3(c[18..]) >> 3)
	c8 := 2097151 & load3(c[21..])
	c9 := 2097151 & (load4(c[23..]) >> 5)
	c10 := 2097151 & (load3(c[26..]) >> 2)
	c11 := (load4(c[28..]) >> 7)

	mut carry := [23]i64{} // original one
	// mut carry := [23]u64{}

	mut s0 := c0 + a0 * b0
	mut s1 := c1 + a0 * b1 + a1 * b0
	mut s2 := c2 + a0 * b2 + a1 * b1 + a2 * b0
	mut s3 := c3 + a0 * b3 + a1 * b2 + a2 * b1 + a3 * b0
	mut s4 := c4 + a0 * b4 + a1 * b3 + a2 * b2 + a3 * b1 + a4 * b0
	mut s5 := c5 + a0 * b5 + a1 * b4 + a2 * b3 + a3 * b2 + a4 * b1 + a5 * b0
	mut s6 := c6 + a0 * b6 + a1 * b5 + a2 * b4 + a3 * b3 + a4 * b2 + a5 * b1 + a6 * b0
	mut s7 := c7 + a0 * b7 + a1 * b6 + a2 * b5 + a3 * b4 + a4 * b3 + a5 * b2 + a6 * b1 + a7 * b0
	mut s8 := c8 + a0 * b8 + a1 * b7 + a2 * b6 + a3 * b5 + a4 * b4 + a5 * b3 + a6 * b2 + a7 * b1 +
		a8 * b0
	mut s9 := c9 + a0 * b9 + a1 * b8 + a2 * b7 + a3 * b6 + a4 * b5 + a5 * b4 + a6 * b3 + a7 * b2 +
		a8 * b1 + a9 * b0
	mut s10 := c10 + a0 * b10 + a1 * b9 + a2 * b8 + a3 * b7 + a4 * b6 + a5 * b5 + a6 * b4 +
		a7 * b3 + a8 * b2 + a9 * b1 + a10 * b0
	mut s11 := c11 + a0 * b11 + a1 * b10 + a2 * b9 + a3 * b8 + a4 * b7 + a5 * b6 + a6 * b5 +
		a7 * b4 + a8 * b3 + a9 * b2 + a10 * b1 + a11 * b0
	mut s12 := a1 * b11 + a2 * b10 + a3 * b9 + a4 * b8 + a5 * b7 + a6 * b6 + a7 * b5 + a8 * b4 +
		a9 * b3 + a10 * b2 + a11 * b1
	mut s13 := a2 * b11 + a3 * b10 + a4 * b9 + a5 * b8 + a6 * b7 + a7 * b6 + a8 * b5 + a9 * b4 +
		a10 * b3 + a11 * b2
	mut s14 := a3 * b11 + a4 * b10 + a5 * b9 + a6 * b8 + a7 * b7 + a8 * b6 + a9 * b5 + a10 * b4 +
		a11 * b3
	mut s15 := a4 * b11 + a5 * b10 + a6 * b9 + a7 * b8 + a8 * b7 + a9 * b6 + a10 * b5 + a11 * b4
	mut s16 := a5 * b11 + a6 * b10 + a7 * b9 + a8 * b8 + a9 * b7 + a10 * b6 + a11 * b5
	mut s17 := a6 * b11 + a7 * b10 + a8 * b9 + a9 * b8 + a10 * b7 + a11 * b6
	mut s18 := a7 * b11 + a8 * b10 + a9 * b9 + a10 * b8 + a11 * b7
	mut s19 := a8 * b11 + a9 * b10 + a10 * b9 + a11 * b8
	mut s20 := a9 * b11 + a10 * b10 + a11 * b9
	mut s21 := a10 * b11 + a11 * b10
	mut s22 := a11 * b11

	mut s23 := i64(0) // original
	// mut s23 := u64(0)

	// carry[0] = (s0 + (1048576)) >> 21
	carry[0] = (s0 + (1048576)) >> 21
	s1 += carry[0]
	s0 -= carry[0] * 2097152
	carry[2] = (s2 + (1048576)) >> 21
	s3 += carry[2]
	s2 -= carry[2] * 2097152
	carry[4] = (s4 + (1048576)) >> 21
	s5 += carry[4]
	s4 -= carry[4] * 2097152
	carry[6] = (s6 + (1048576)) >> 21
	s7 += carry[6]
	s6 -= carry[6] * 2097152
	carry[8] = (s8 + (1048576)) >> 21
	s9 += carry[8]
	s8 -= carry[8] * 2097152
	carry[10] = (s10 + (1048576)) >> 21
	s11 += carry[10]
	s10 -= carry[10] * 2097152
	carry[12] = (s12 + (1048576)) >> 21
	s13 += carry[12]
	s12 -= carry[12] * 2097152
	carry[14] = (s14 + (1048576)) >> 21
	s15 += carry[14]
	s14 -= carry[14] * 2097152
	carry[16] = (s16 + (1048576)) >> 21
	s17 += carry[16]
	s16 -= carry[16] * 2097152
	carry[18] = (s18 + (1048576)) >> 21
	s19 += carry[18]
	s18 -= carry[18] * 2097152
	carry[20] = (s20 + (1048576)) >> 21
	s21 += carry[20]
	s20 -= carry[20] * 2097152
	carry[22] = (s22 + (1048576)) >> 21
	s23 += carry[22]
	s22 -= carry[22] * 2097152

	carry[1] = (s1 + (1048576)) >> 21
	s2 += carry[1]
	s1 -= carry[1] * 2097152
	carry[3] = (s3 + (1048576)) >> 21
	s4 += carry[3]
	s3 -= carry[3] * 2097152
	carry[5] = (s5 + (1048576)) >> 21
	s6 += carry[5]
	s5 -= carry[5] * 2097152
	carry[7] = (s7 + (1048576)) >> 21
	s8 += carry[7]
	s7 -= carry[7] * 2097152
	carry[9] = (s9 + (1048576)) >> 21
	s10 += carry[9]
	s9 -= carry[9] * 2097152
	carry[11] = (s11 + (1048576)) >> 21
	s12 += carry[11]
	s11 -= carry[11] * 2097152
	carry[13] = (s13 + (1048576)) >> 21
	s14 += carry[13]
	s13 -= carry[13] * 2097152
	carry[15] = (s15 + (1048576)) >> 21
	s16 += carry[15]
	s15 -= carry[15] * 2097152
	carry[17] = (s17 + (1048576)) >> 21
	s18 += carry[17]
	s17 -= carry[17] * 2097152
	carry[19] = (s19 + (1048576)) >> 21
	s20 += carry[19]
	s19 -= carry[19] * 2097152
	carry[21] = (s21 + (1048576)) >> 21
	s22 += carry[21]
	s21 -= carry[21] * 2097152

	s11 += s23 * 666643
	s12 += s23 * 470296
	s13 += s23 * 654183
	s14 -= s23 * 997805
	s15 += s23 * 136657
	s16 -= s23 * 683901
	s23 = 0

	s10 += s22 * 666643
	s11 += s22 * 470296
	s12 += s22 * 654183
	s13 -= s22 * 997805
	s14 += s22 * 136657
	s15 -= s22 * 683901
	s22 = 0

	s9 += s21 * 666643
	s10 += s21 * 470296
	s11 += s21 * 654183
	s12 -= s21 * 997805
	s13 += s21 * 136657
	s14 -= s21 * 683901
	s21 = 0

	s8 += s20 * 666643
	s9 += s20 * 470296
	s10 += s20 * 654183
	s11 -= s20 * 997805
	s12 += s20 * 136657
	s13 -= s20 * 683901
	s20 = 0

	s7 += s19 * 666643
	s8 += s19 * 470296
	s9 += s19 * 654183
	s10 -= s19 * 997805
	s11 += s19 * 136657
	s12 -= s19 * 683901
	s19 = 0

	s6 += s18 * 666643
	s7 += s18 * 470296
	s8 += s18 * 654183
	s9 -= s18 * 997805
	s10 += s18 * 136657
	s11 -= s18 * 683901
	s18 = 0

	carry[6] = (s6 + (1048576)) >> 21
	s7 += carry[6]
	s6 -= carry[6] * 2097152
	carry[8] = (s8 + (1048576)) >> 21
	s9 += carry[8]
	s8 -= carry[8] * 2097152
	carry[10] = (s10 + (1048576)) >> 21
	s11 += carry[10]
	s10 -= carry[10] * 2097152
	carry[12] = (s12 + (1048576)) >> 21
	s13 += carry[12]
	s12 -= carry[12] * 2097152
	carry[14] = (s14 + (1048576)) >> 21
	s15 += carry[14]
	s14 -= carry[14] * 2097152
	carry[16] = (s16 + (1048576)) >> 21
	s17 += carry[16]
	s16 -= carry[16] * 2097152

	carry[7] = (s7 + (1048576)) >> 21
	s8 += carry[7]
	s7 -= carry[7] * 2097152
	carry[9] = (s9 + (1048576)) >> 21
	s10 += carry[9]
	s9 -= carry[9] * 2097152
	carry[11] = (s11 + (1048576)) >> 21
	s12 += carry[11]
	s11 -= carry[11] * 2097152
	carry[13] = (s13 + (1048576)) >> 21
	s14 += carry[13]
	s13 -= carry[13] * 2097152
	carry[15] = (s15 + (1048576)) >> 21
	s16 += carry[15]
	s15 -= carry[15] * 2097152

	s5 += s17 * 666643
	s6 += s17 * 470296
	s7 += s17 * 654183
	s8 -= s17 * 997805
	s9 += s17 * 136657
	s10 -= s17 * 683901
	s17 = 0

	s4 += s16 * 666643
	s5 += s16 * 470296
	s6 += s16 * 654183
	s7 -= s16 * 997805
	s8 += s16 * 136657
	s9 -= s16 * 683901
	s16 = 0

	s3 += s15 * 666643
	s4 += s15 * 470296
	s5 += s15 * 654183
	s6 -= s15 * 997805
	s7 += s15 * 136657
	s8 -= s15 * 683901
	s15 = 0

	s2 += s14 * 666643
	s3 += s14 * 470296
	s4 += s14 * 654183
	s5 -= s14 * 997805
	s6 += s14 * 136657
	s7 -= s14 * 683901
	s14 = 0

	s1 += s13 * 666643
	s2 += s13 * 470296
	s3 += s13 * 654183
	s4 -= s13 * 997805
	s5 += s13 * 136657
	s6 -= s13 * 683901
	s13 = 0

	s0 += s12 * 666643
	s1 += s12 * 470296
	s2 += s12 * 654183
	s3 -= s12 * 997805
	s4 += s12 * 136657
	s5 -= s12 * 683901
	s12 = 0

	carry[0] = (s0 + (1048576)) >> 21
	s1 += carry[0]
	s0 -= carry[0] * 2097152
	carry[2] = (s2 + (1048576)) >> 21
	s3 += carry[2]
	s2 -= carry[2] * 2097152
	carry[4] = (s4 + (1048576)) >> 21
	s5 += carry[4]
	s4 -= carry[4] * 2097152
	carry[6] = (s6 + (1048576)) >> 21
	s7 += carry[6]
	s6 -= carry[6] * 2097152
	carry[8] = (s8 + (1048576)) >> 21
	s9 += carry[8]
	s8 -= carry[8] * 2097152
	carry[10] = (s10 + (1048576)) >> 21
	s11 += carry[10]
	s10 -= carry[10] * 2097152

	carry[1] = (s1 + (1048576)) >> 21
	s2 += carry[1]
	s1 -= carry[1] * 2097152
	carry[3] = (s3 + (1048576)) >> 21
	s4 += carry[3]
	s3 -= carry[3] * 2097152
	carry[5] = (s5 + (1048576)) >> 21
	s6 += carry[5]
	s5 -= carry[5] * 2097152
	carry[7] = (s7 + (1048576)) >> 21
	s8 += carry[7]
	s7 -= carry[7] * 2097152
	carry[9] = (s9 + (1048576)) >> 21
	s10 += carry[9]
	s9 -= carry[9] * 2097152
	carry[11] = (s11 + (1048576)) >> 21
	s12 += carry[11]
	s11 -= carry[11] * 2097152

	s0 += s12 * 666643
	s1 += s12 * 470296
	s2 += s12 * 654183
	s3 -= s12 * 997805
	s4 += s12 * 136657
	s5 -= s12 * 683901
	s12 = 0

	carry[0] = s0 >> 21
	s1 += carry[0]
	s0 -= carry[0] * 2097152
	carry[1] = s1 >> 21
	s2 += carry[1]
	s1 -= carry[1] * 2097152
	carry[2] = s2 >> 21
	s3 += carry[2]
	s2 -= carry[2] * 2097152
	carry[3] = s3 >> 21
	s4 += carry[3]
	s3 -= carry[3] * 2097152
	carry[4] = s4 >> 21
	s5 += carry[4]
	s4 -= carry[4] * 2097152
	carry[5] = s5 >> 21
	s6 += carry[5]
	s5 -= carry[5] * 2097152
	carry[6] = s6 >> 21
	s7 += carry[6]
	s6 -= carry[6] * 2097152
	carry[7] = s7 >> 21
	s8 += carry[7]
	s7 -= carry[7] * 2097152
	carry[8] = s8 >> 21
	s9 += carry[8]
	s8 -= carry[8] * 2097152
	carry[9] = s9 >> 21
	s10 += carry[9]
	s9 -= carry[9] * 2097152
	carry[10] = s10 >> 21
	s11 += carry[10]
	s10 -= carry[10] * 2097152
	carry[11] = s11 >> 21
	s12 += carry[11]
	s11 -= carry[11] * 2097152

	s0 += s12 * 666643
	s1 += s12 * 470296
	s2 += s12 * 654183
	s3 -= s12 * 997805
	s4 += s12 * 136657
	s5 -= s12 * 683901
	s12 = 0

	carry[0] = s0 >> 21
	s1 += carry[0]
	s0 -= carry[0] * 2097152
	carry[1] = s1 >> 21
	s2 += carry[1]
	s1 -= carry[1] * 2097152
	carry[2] = s2 >> 21
	s3 += carry[2]
	s2 -= carry[2] * 2097152
	carry[3] = s3 >> 21
	s4 += carry[3]
	s3 -= carry[3] * 2097152
	carry[4] = s4 >> 21
	s5 += carry[4]
	s4 -= carry[4] * 2097152
	carry[5] = s5 >> 21
	s6 += carry[5]
	s5 -= carry[5] * 2097152
	carry[6] = s6 >> 21
	s7 += carry[6]
	s6 -= carry[6] * 2097152
	carry[7] = s7 >> 21
	s8 += carry[7]
	s7 -= carry[7] * 2097152
	carry[8] = s8 >> 21
	s9 += carry[8]
	s8 -= carry[8] * 2097152
	carry[9] = s9 >> 21
	s10 += carry[9]
	s9 -= carry[9] * 2097152
	carry[10] = s10 >> 21
	s11 += carry[10]
	s10 -= carry[10] * 2097152

	s[0] = u8(s0 >> 0)
	s[1] = u8(s0 >> 8)
	s[2] = u8((s0 >> 16) | (s1 * 32))
	s[3] = u8(s1 >> 3)
	s[4] = u8(s1 >> 11)
	s[5] = u8((s1 >> 19) | (s2 * 4))
	s[6] = u8(s2 >> 6)
	s[7] = u8((s2 >> 14) | (s3 * 128))
	s[8] = u8(s3 >> 1)
	s[9] = u8(s3 >> 9)
	s[10] = u8((s3 >> 17) | (s4 * 16))
	s[11] = u8(s4 >> 4)
	s[12] = u8(s4 >> 12)
	s[13] = u8((s4 >> 20) | (s5 * 2))
	s[14] = u8(s5 >> 7)
	s[15] = u8((s5 >> 15) | (s6 * 64))
	s[16] = u8(s6 >> 2)
	s[17] = u8(s6 >> 10)
	s[18] = u8((s6 >> 18) | (s7 * 8))
	s[19] = u8(s7 >> 5)
	s[20] = u8(s7 >> 13)
	s[21] = u8(s8 >> 0)
	s[22] = u8(s8 >> 8)
	s[23] = u8((s8 >> 16) | (s9 * 32))
	s[24] = u8(s9 >> 3)
	s[25] = u8(s9 >> 11)
	s[26] = u8((s9 >> 19) | (s10 * 4))
	s[27] = u8(s10 >> 6)
	s[28] = u8((s10 >> 14) | (s11 * 128))
	s[29] = u8(s11 >> 1)
	s[30] = u8(s11 >> 9)
	s[31] = u8(s11 >> 17)
}

// Input:
//   s[0]+256*s[1]+...+256^63*s[63] = s
//
// Output:
//   s[0]+256*s[1]+...+256^31*s[31] = s mod l
//   where l = 2^252 + 27742317777372353535851937790883648493.
fn sc_reduce(mut out [32]u8, mut s []u8) {
	assert out.len == 32
	assert s.len == 64
	mut s0 := 2097151 & load3(s[..])
	mut s1 := 2097151 & (load4(s[2..]) >> 5)
	mut s2 := 2097151 & (load3(s[5..]) >> 2)
	mut s3 := 2097151 & (load4(s[7..]) >> 7)
	mut s4 := 2097151 & (load4(s[10..]) >> 4)
	mut s5 := 2097151 & (load3(s[13..]) >> 1)
	mut s6 := 2097151 & (load4(s[15..]) >> 6)
	mut s7 := 2097151 & (load3(s[18..]) >> 3)
	mut s8 := 2097151 & load3(s[21..])
	mut s9 := 2097151 & (load4(s[23..]) >> 5)
	mut s10 := 2097151 & (load3(s[26..]) >> 2)
	mut s11 := 2097151 & (load4(s[28..]) >> 7)
	mut s12 := 2097151 & (load4(s[31..]) >> 4)
	mut s13 := 2097151 & (load3(s[34..]) >> 1)
	mut s14 := 2097151 & (load4(s[36..]) >> 6)
	mut s15 := 2097151 & (load3(s[39..]) >> 3)
	mut s16 := 2097151 & load3(s[42..])
	mut s17 := 2097151 & (load4(s[44..]) >> 5)
	mut s18 := 2097151 & (load3(s[47..]) >> 2)
	mut s19 := 2097151 & (load4(s[49..]) >> 7)
	mut s20 := 2097151 & (load4(s[52..]) >> 4)
	mut s21 := 2097151 & (load3(s[55..]) >> 1)
	mut s22 := 2097151 & (load4(s[57..]) >> 6)
	mut s23 := (load4(s[60..]) >> 3)

	s11 += s23 * 666643
	s12 += s23 * 470296
	s13 += s23 * 654183
	s14 -= s23 * 997805
	s15 += s23 * 136657
	s16 -= s23 * 683901
	s23 = 0

	s10 += s22 * 666643
	s11 += s22 * 470296
	s12 += s22 * 654183
	s13 -= s22 * 997805
	s14 += s22 * 136657
	s15 -= s22 * 683901
	s22 = 0

	s9 += s21 * 666643
	s10 += s21 * 470296
	s11 += s21 * 654183
	s12 -= s21 * 997805
	s13 += s21 * 136657
	s14 -= s21 * 683901
	s21 = 0

	s8 += s20 * 666643
	s9 += s20 * 470296
	s10 += s20 * 654183
	s11 -= s20 * 997805
	s12 += s20 * 136657
	s13 -= s20 * 683901
	s20 = 0

	s7 += s19 * 666643
	s8 += s19 * 470296
	s9 += s19 * 654183
	s10 -= s19 * 997805
	s11 += s19 * 136657
	s12 -= s19 * 683901
	s19 = 0

	s6 += s18 * 666643
	s7 += s18 * 470296
	s8 += s18 * 654183
	s9 -= s18 * 997805
	s10 += s18 * 136657
	s11 -= s18 * 683901
	s18 = 0

	mut carry := [17]i64{} // original one
	// mut carry := [17]u64{}

	carry[6] = (s6 + (1048576)) >> 21
	s7 += carry[6]
	s6 -= carry[6] * 2097152
	carry[8] = (s8 + (1048576)) >> 21
	s9 += carry[8]
	s8 -= carry[8] * 2097152
	carry[10] = (s10 + (1048576)) >> 21
	s11 += carry[10]
	s10 -= carry[10] * 2097152
	carry[12] = (s12 + (1048576)) >> 21
	s13 += carry[12]
	s12 -= carry[12] * 2097152
	carry[14] = (s14 + (1048576)) >> 21
	s15 += carry[14]
	s14 -= carry[14] * 2097152
	carry[16] = (s16 + (1048576)) >> 21
	s17 += carry[16]
	s16 -= carry[16] * 2097152

	carry[7] = (s7 + (1048576)) >> 21
	s8 += carry[7]
	s7 -= carry[7] * 2097152
	carry[9] = (s9 + (1048576)) >> 21
	s10 += carry[9]
	s9 -= carry[9] * 2097152
	carry[11] = (s11 + (1048576)) >> 21
	s12 += carry[11]
	s11 -= carry[11] * 2097152
	carry[13] = (s13 + (1048576)) >> 21
	s14 += carry[13]
	s13 -= carry[13] * 2097152
	carry[15] = (s15 + (1048576)) >> 21
	s16 += carry[15]
	s15 -= carry[15] * 2097152

	s5 += s17 * 666643
	s6 += s17 * 470296
	s7 += s17 * 654183
	s8 -= s17 * 997805
	s9 += s17 * 136657
	s10 -= s17 * 683901
	s17 = 0

	s4 += s16 * 666643
	s5 += s16 * 470296
	s6 += s16 * 654183
	s7 -= s16 * 997805
	s8 += s16 * 136657
	s9 -= s16 * 683901
	s16 = 0

	s3 += s15 * 666643
	s4 += s15 * 470296
	s5 += s15 * 654183
	s6 -= s15 * 997805
	s7 += s15 * 136657
	s8 -= s15 * 683901
	s15 = 0

	s2 += s14 * 666643
	s3 += s14 * 470296
	s4 += s14 * 654183
	s5 -= s14 * 997805
	s6 += s14 * 136657
	s7 -= s14 * 683901
	s14 = 0

	s1 += s13 * 666643
	s2 += s13 * 470296
	s3 += s13 * 654183
	s4 -= s13 * 997805
	s5 += s13 * 136657
	s6 -= s13 * 683901
	s13 = 0

	s0 += s12 * 666643
	s1 += s12 * 470296
	s2 += s12 * 654183
	s3 -= s12 * 997805
	s4 += s12 * 136657
	s5 -= s12 * 683901
	s12 = 0

	carry[0] = (s0 + (1048576)) >> 21
	s1 += carry[0]
	s0 -= carry[0] * 2097152
	carry[2] = (s2 + (1048576)) >> 21
	s3 += carry[2]
	s2 -= carry[2] * 2097152
	carry[4] = (s4 + (1048576)) >> 21
	s5 += carry[4]
	s4 -= carry[4] * 2097152
	carry[6] = (s6 + (1048576)) >> 21
	s7 += carry[6]
	s6 -= carry[6] * 2097152
	carry[8] = (s8 + (1048576)) >> 21
	s9 += carry[8]
	s8 -= carry[8] * 2097152
	carry[10] = (s10 + (1048576)) >> 21
	s11 += carry[10]
	s10 -= carry[10] * 2097152

	carry[1] = (s1 + (1048576)) >> 21
	s2 += carry[1]
	s1 -= carry[1] * 2097152
	carry[3] = (s3 + (1048576)) >> 21
	s4 += carry[3]
	s3 -= carry[3] * 2097152
	carry[5] = (s5 + (1048576)) >> 21
	s6 += carry[5]
	s5 -= carry[5] * 2097152
	carry[7] = (s7 + (1048576)) >> 21
	s8 += carry[7]
	s7 -= carry[7] * 2097152
	carry[9] = (s9 + (1048576)) >> 21
	s10 += carry[9]
	s9 -= carry[9] * 2097152
	carry[11] = (s11 + (1048576)) >> 21
	s12 += carry[11]
	s11 -= carry[11] * 2097152

	s0 += s12 * 666643
	s1 += s12 * 470296
	s2 += s12 * 654183
	s3 -= s12 * 997805
	s4 += s12 * 136657
	s5 -= s12 * 683901
	s12 = 0

	carry[0] = s0 >> 21
	s1 += carry[0]
	s0 -= carry[0] * 2097152
	carry[1] = s1 >> 21
	s2 += carry[1]
	s1 -= carry[1] * 2097152
	carry[2] = s2 >> 21
	s3 += carry[2]
	s2 -= carry[2] * 2097152
	carry[3] = s3 >> 21
	s4 += carry[3]
	s3 -= carry[3] * 2097152
	carry[4] = s4 >> 21
	s5 += carry[4]
	s4 -= carry[4] * 2097152
	carry[5] = s5 >> 21
	s6 += carry[5]
	s5 -= carry[5] * 2097152
	carry[6] = s6 >> 21
	s7 += carry[6]
	s6 -= carry[6] * 2097152
	carry[7] = s7 >> 21
	s8 += carry[7]
	s7 -= carry[7] * 2097152
	carry[8] = s8 >> 21
	s9 += carry[8]
	s8 -= carry[8] * 2097152
	carry[9] = s9 >> 21
	s10 += carry[9]
	s9 -= carry[9] * 2097152
	carry[10] = s10 >> 21
	s11 += carry[10]
	s10 -= carry[10] * 2097152
	carry[11] = s11 >> 21
	s12 += carry[11]
	s11 -= carry[11] * 2097152

	s0 += s12 * 666643
	s1 += s12 * 470296
	s2 += s12 * 654183
	s3 -= s12 * 997805
	s4 += s12 * 136657
	s5 -= s12 * 683901
	s12 = 0

	carry[0] = s0 >> 21
	s1 += carry[0]
	s0 -= carry[0] * 2097152
	carry[1] = s1 >> 21
	s2 += carry[1]
	s1 -= carry[1] * 2097152
	carry[2] = s2 >> 21
	s3 += carry[2]
	s2 -= carry[2] * 2097152
	carry[3] = s3 >> 21
	s4 += carry[3]
	s3 -= carry[3] * 2097152
	carry[4] = s4 >> 21
	s5 += carry[4]
	s4 -= carry[4] * 2097152
	carry[5] = s5 >> 21
	s6 += carry[5]
	s5 -= carry[5] * 2097152
	carry[6] = s6 >> 21
	s7 += carry[6]
	s6 -= carry[6] * 2097152
	carry[7] = s7 >> 21
	s8 += carry[7]
	s7 -= carry[7] * 2097152
	carry[8] = s8 >> 21
	s9 += carry[8]
	s8 -= carry[8] * 2097152
	carry[9] = s9 >> 21
	s10 += carry[9]
	s9 -= carry[9] * 2097152
	carry[10] = s10 >> 21
	s11 += carry[10]
	s10 -= carry[10] * 2097152

	out[0] = u8(s0 >> 0)
	out[1] = u8(s0 >> 8)
	out[2] = u8((s0 >> 16) | (s1 * 32))
	out[3] = u8(s1 >> 3)
	out[4] = u8(s1 >> 11)
	out[5] = u8((s1 >> 19) | (s2 * 4))
	out[6] = u8(s2 >> 6)
	out[7] = u8((s2 >> 14) | (s3 * 128))
	out[8] = u8(s3 >> 1)
	out[9] = u8(s3 >> 9)
	out[10] = u8((s3 >> 17) | (s4 * 16))
	out[11] = u8(s4 >> 4)
	out[12] = u8(s4 >> 12)
	out[13] = u8((s4 >> 20) | (s5 * 2))
	out[14] = u8(s5 >> 7)
	out[15] = u8((s5 >> 15) | (s6 * 64))
	out[16] = u8(s6 >> 2)
	out[17] = u8(s6 >> 10)
	out[18] = u8((s6 >> 18) | (s7 * 8))
	out[19] = u8(s7 >> 5)
	out[20] = u8(s7 >> 13)
	out[21] = u8(s8 >> 0)
	out[22] = u8(s8 >> 8)
	out[23] = u8((s8 >> 16) | (s9 * 32))
	out[24] = u8(s9 >> 3)
	out[25] = u8(s9 >> 11)
	out[26] = u8((s9 >> 19) | (s10 * 4))
	out[27] = u8(s10 >> 6)
	out[28] = u8((s10 >> 14) | (s11 * 128))
	out[29] = u8(s11 >> 1)
	out[30] = u8(s11 >> 9)
	out[31] = u8(s11 >> 17)
}

// non_adjacent_form computes a width-w non-adjacent form for this scalar.
//
// w must be between 2 and 8, or non_adjacent_form will panic.
pub fn (mut s Scalar) non_adjacent_form(w u32) []i8 {
	// This implementation is adapted from the one
	// in curve25519-dalek and is documented there:
	// https://github.com/dalek-cryptography/curve25519-dalek/blob/f630041af28e9a405255f98a8a93adca18e4315b/src/scalar.rs#L800-L871
	if s.s[31] > 127 {
		panic('scalar has high bit set illegally')
	}
	if w < 2 {
		panic('w must be at least 2 by the definition of NAF')
	} else if w > 8 {
		panic('NAF digits must fit in i8')
	}

	mut naf := []i8{len: 256}
	mut digits := [5]u64{}

	for i := 0; i < 4; i++ {
		digits[i] = binary.little_endian_u64(s.s[i * 8..])
	}

	width := u64(1 << w)
	window_mask := u64(width - 1)

	mut pos := u32(0)
	mut carry := u64(0)
	for pos < 256 {
		idx_64 := pos / 64
		idx_bit := pos % 64
		mut bitbuf := u64(0)
		if idx_bit < 64 - w {
			// This window's bits are contained in a single u64
			bitbuf = digits[idx_64] >> idx_bit
		} else {
			// Combine the current 64 bits with bits from the next 64
			bitbuf = (digits[idx_64] >> idx_bit) | (digits[1 + idx_64] << (64 - idx_bit))
		}

		// Add carry into the current window
		window := carry + (bitbuf & window_mask)

		if window & 1 == 0 {
			// If the window value is even, preserve the carry and continue.
			// Why is the carry preserved?
			// If carry == 0 and window & 1 == 0,
			//    then the next carry should be 0
			// If carry == 1 and window & 1 == 0,
			//    then bit_buf & 1 == 1 so the next carry should be 1
			pos += 1
			continue
		}

		if window < width / 2 {
			carry = 0
			naf[pos] = i8(window)
		} else {
			carry = 1
			naf[pos] = i8(window) - i8(width)
		}

		pos += w
	}
	return naf
}

fn (mut s Scalar) signed_radix16() []i8 {
	if s.s[31] > 127 {
		panic('scalar has high bit set illegally')
	}

	mut digits := []i8{len: 64}

	// Compute unsigned radix-16 digits:
	for i := 0; i < 32; i++ {
		digits[2 * i] = i8(s.s[i] & 15)
		digits[2 * i + 1] = i8((s.s[i] >> 4) & 15)
	}

	// Recenter coefficients:
	for i := 0; i < 63; i++ {
		mut carry := (digits[i] + 8) >> 4

		// digits[i] -= unsafe { carry * 16 } // original one
		digits[i] -= unsafe { carry * 16 } // carry * 16 == carry *

		digits[i + 1] += carry
	}

	return digits
}

// utility function
// generate returns a valid (reduced modulo l) Scalar with a distribution
// weighted towards high, low, and edge values.
fn generate_scalar(size int) !Scalar {
	/*
	s := scZero
	diceRoll := rand.Intn(100)
	switch {
	case diceRoll == 0:
	case diceRoll == 1:
		s = scOne
	case diceRoll == 2:
		s = scMinusOne
	case diceRoll < 5:
		// Generate a low scalar in [0, 2^125).
		rand.Read(s.s[:16])
		s.s[15] &= (1 * 32) - 1
	case diceRoll < 10:
		// Generate a high scalar in [2^252, 2^252 + 2^124).
		s.s[31] = 1 * 16
		rand.Read(s.s[:16])
		s.s[15] &= (1 * 16) - 1
	default:
		// Generate a valid scalar in [0, l) by returning [0, 2^252) which has a
		// negligibly different distribution (the former has a 2^-127.6 chance
		// of being out of the latter range).
		rand.Read(s.s[:])
		s.s[31] &= (1 * 16) - 1
	}
	return reflect.ValueOf(s)
	*/
	mut s := edwards25519.sc_zero
	diceroll := rand.intn(100) or { 0 }
	match true {
		/*
		case diceroll == 0:
			case diceroll == 1:
		*/
		diceroll == 0 || diceroll == 1 {
			s = edwards25519.sc_one
		}
		diceroll == 2 {
			s = edwards25519.sc_minus_one
		}
		diceroll < 5 {
			// rand.Read(s.s[:16]) // read random bytes and fill buf
			// using builtin rand.read([]buf)
			rand.read(mut s.s[..16])
			// buf := rand.read(s.s[..16].len)!
			// copy(mut s.s[..16], buf)

			/*
			for i, item in buf {
				s.s[i] = item
			}
			*/
			s.s[15] &= (1 * 32) - 1
			// generate a low scalar in [0, 2^125).
		}
		diceroll < 10 {
			// generate a high scalar in [2^252, 2^252 + 2^124).
			s.s[31] = 1 * 16
			// Read generates len(p) random bytes and writes them into p
			// rand.Read(s.s[:16])
			rand.read(mut s.s[..16])
			// buf := rand.read(s.s[..16].len)!
			// copy(mut s.s[..16], buf)

			/*
			for i, item in buf {
				s.s[i] = item
			}
			*/
			s.s[15] &= (1 * 16) - 1
		}
		else {
			// generate a valid scalar in [0, l) by returning [0, 2^252) which has a
			// negligibly different distribution (the former has a 2^-127.6 chance
			// of being out of the latter range).
			// rand.Read(s.s[:])
			rand.read(mut s.s[..])
			// buf := crand.read(s.s.len)!
			// copy(mut s.s[..], buf)

			/*
			for i, item in buf {
				s.s[i] = item
			}
			*/
			s.s[31] &= (1 * 16) - 1
		}
	}
	return s
}

type NotZeroScalar = Scalar

fn generate_notzero_scalar(size int) !NotZeroScalar {
	mut s := Scalar{}
	for s == edwards25519.sc_zero {
		s = generate_scalar(size)!
	}
	return NotZeroScalar(s)
}
