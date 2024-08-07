module poly1305

import math.bits
import math.unsigned

const uint192_zero = Uint192{}

// Uint192 is a structure representing 192 bits of unsigned integer.
// It serves as a custom allocator for the poly1305 implementation to use.
// However, it can be used to complement and expand the standard `math.unsigned`
// module with a more complete and extensive range of handling of unsigned integers.
struct Uint192 {
mut:
	lo u64
	mi u64
	hi u64
}

// Here, we define several required functionalities of the custom allocator.

// add_checked returns u+v with carry
fn (u Uint192) add_checked(v Uint192, c u64) (Uint192, u64) {
	lo, c0 := bits.add_64(u.lo, v.lo, c)
	mi, c1 := bits.add_64(u.mi, v.mi, c0)
	hi, c2 := bits.add_64(u.hi, v.hi, c1)
	x := Uint192{lo, mi, hi}
	return x, c2
}

// add_128_checked returns u+v with carry
fn (u Uint192) add_128_checked(v unsigned.Uint128, c u64) (Uint192, u64) {
	lo, c0 := bits.add_64(u.lo, v.lo, c)
	mi, c1 := bits.add_64(u.mi, v.hi, c0)
	hi, c2 := bits.add_64(u.hi, 0, c1)
	x := Uint192{lo, mi, hi}
	return x, c2
}

fn (u Uint192) add_64_checked(v u64, c u64) (Uint192, u64) {
	lo, c0 := bits.add_64(u.lo, v, c)
	mi, c1 := bits.add_64(u.mi, 0, c0)
	hi, c2 := bits.add_64(u.hi, 0, c1)
	x := Uint192{lo, mi, hi}
	return x, c2
}

fn (u Uint192) sub_checked(v Uint192) (Uint192, u64) {
	lo, b0 := bits.sub_64(u.lo, v.lo, 0)
	mi, b1 := bits.sub_64(u.mi, v.mi, b0)
	hi, b2 := bits.sub_64(u.hi, v.hi, b1)
	return Uint192{lo, mi, hi}, b2
}

// mul_64_checked returns `u*v` even if the result size is over > 192 bits.
// It returns a `(Uin192, u64)` pair where the former stores the low 192 bits and
// the rest of high bits stored in the `u64` part. You can check the value of the `u64` part
// for `c != 0`, it's mean, the product of `u*v` is overflowing 192 bits.
fn (u Uint192) mul_64_checked(v u64) (Uint192, u64) {
	// see mul_128_checked for the logic
	m0 := u128_from_64_mul(u.lo, v)
	m1 := u128_from_64_mul(u.mi, v)
	m2 := u128_from_64_mul(u.hi, v)
	// propagates carry
	t0, c0 := bits.add_64(m0.lo, 0, 0)
	t1, c1 := bits.add_64(m0.hi, m1.lo, c0)
	t2, c2 := bits.add_64(m1.hi, m2.lo, c1)
	t3, c3 := bits.add_64(m2.hi, 0, c2)
	// something bad happen if the last c3 is non null
	if c3 != 0 {
		panic('Uint192: unexpected overflow')
	}
	x := Uint192{
		lo: t0
		mi: t1
		hi: t2
	}
	return x, t3
}

// mul_128_checked is a generic product of 192 bits u by 128 bits v.
// Its returns u*v even if the result is over > 192 bits, and stores the remaining
// high bits of the result into the Uint128 structure.
fn (u Uint192) mul_128_checked(v unsigned.Uint128) (Uint192, unsigned.Uint128) {
	//                  u.hi         u.mi        u.lo
	//                               v.hi        v.lo
	// ---------------------------------------------------------- x
	//                  uhi*vlo      umi*vlo     ulo*vlo
	//      uhi*vhi     umi*vhi      ulo*vhi
	// ==========================================================
	//      m3          m2           m1          m0                // Uint128
	//
	// ---------------------------------------------------------- +
	//      m3.hi       m2.hi       m1.hi        m0.hi
	//                  m3.lo       m2.lo        m1.lo     m0.lo
	// ==========================================================
	//      t4          t3          t2           t1        t0
	//
	ulovlo := u128_from_64_mul(u.lo, v.lo)
	umivlo := u128_from_64_mul(u.mi, v.lo)
	uhivlo := u128_from_64_mul(u.hi, v.lo)

	ulovhi := u128_from_64_mul(u.lo, v.hi)
	umivhi := u128_from_64_mul(u.mi, v.hi)
	uhivhi := u128_from_64_mul(u.hi, v.hi)

	m0 := ulovlo
	m1, c1 := unsigned.add_128(umivlo, ulovhi, 0)
	m2, c2 := unsigned.add_128(uhivlo, umivhi, c1)
	m3, c3 := unsigned.add_128(uhivhi, unsigned.uint128_zero, c2)
	if c3 != 0 {
		panic('Uint192: unexpected overflow')
	}
	// Note about the multiplication results in Poly1305 context.
	// In the properly clamped 128 bits of v, (called "r" in the poly1305 context) and
	// safely reduced form of high part of the 192 bits accumulator u (u.hi), where only
	// maximum of four low bits of u.hi is set, and we can assume (and confirm with tests)
	// if the high bit part of the product of uhi*vhi and uhi*vlo is not set,
	// ie, x = (uhi*vhi).hi == 0 and y = (uhi*vlo).hi == 0.
	// and the 128 bits addition of `m2 = uhi*vlo + umi*vhi`, would also not overflow 128 bits,
	// thats also mean, the last carry is null for the reason and m3 = (uhi*vhi).hi is also null
	//
	t0 := m0.lo
	t1, c4 := bits.add_64(m0.hi, m1.lo, 0)
	t2, c5 := bits.add_64(m1.hi, m2.lo, c4)
	t3, c6 := bits.add_64(m2.hi, m3.lo, c5)
	t4, c7 := bits.add_64(m3.hi, 0, c6)
	if c7 != 0 {
		panic('Uint192: unexpected overflow')
	}
	// based on previous notes, for poly1305 context, it tells us if the product
	// doesn't have a fitfh limb (t4), ie t4 == null, and we can safely ignore it.
	//
	x := Uint192{
		lo: t0
		mi: t1
		hi: t2
	}
	hb := unsigned.uint128_new(t3, t4)
	return x, hb
}

// u128_from_64_mul creates new Uint128 from 64x64 bit product of x*y
fn u128_from_64_mul(x u64, y u64) unsigned.Uint128 {
	hi, lo := bits.mul_64(x, y)
	return unsigned.uint128_new(lo, hi)
}

// select_64 returns x if v == 1 and y if v == 0, in constant time.
fn select_64(v u64, x u64, y u64) u64 {
	return ~(v - 1) & x | (v - 1) & y
}

fn shift_right_by2(mut a unsigned.Uint128) unsigned.Uint128 {
	a.lo = a.lo >> 2 | (a.hi & 3) << 62
	a.hi = a.hi >> 2
	return a
}
