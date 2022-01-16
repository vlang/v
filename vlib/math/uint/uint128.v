module uint

import math.bits

pub struct Uint128 {
mut:
	lo u64
	hi u64
}

pub const (
	uint128_zero = Uint128{}
	uint128_max  = Uint128{18446744073709551615, 18446744073709551615}
)

// is_zero returns true if u == 0
pub fn (u Uint128) is_zero() bool {
	return u == Uint128{}
}

// equals returns true if u == v.
//
// Uint128 values can be compared directly with ==, but use of the Equals method
// is preferred for consistency.
pub fn (u Uint128) equals(v Uint128) bool {
	return u == v
}

// equals_64 returns true if u == v
pub fn (u Uint128) equals_64(v u64) bool {
	return u.lo == v && u.hi == 0
}

// cmp compares u and v and returns:
//
// -1 if u <  v
//  0 if u == v
// +1 if u >  v
//
pub fn (u Uint128) cmp(v Uint128) int {
	if u == v {
		return 0
	} else if u.hi < v.hi || (u.hi == v.hi && u.lo < v.lo) {
		return -1
	} else {
		return 1
	}
}

// cmp_64 compares u and v and returns:
//
//   -1 if u <  v
//    0 if u == v
//   +1 if u >  v
//
pub fn (u Uint128) cmp_64(v u64) int {
	if u.hi == 0 && u.lo == v {
		return 0
	} else if u.hi == 0 && u.lo < v {
		return -1
	} else {
		return 1
	}
}

// and returns u & v
pub fn (u Uint128) and(v Uint128) Uint128 {
	return Uint128{u.lo & v.lo, u.hi & v.hi}
}

// and_64 rreturns u & v
pub fn (u Uint128) and_64(v u64) Uint128 {
	return Uint128{u.lo & v, u.hi & 0}
}

// or returns u | v
pub fn (u Uint128) or_(v Uint128) Uint128 {
	return Uint128{u.lo | v.lo, u.hi | v.hi}
}

// or returns u | v
pub fn (u Uint128) or_64(v u64) Uint128 {
	return Uint128{u.lo | v, u.hi | 0}
}

// xor returns u ^ v
pub fn (u Uint128) xor(v Uint128) Uint128 {
	return Uint128{u.lo ^ v.lo, u.hi ^ v.hi}
}

// xor_64 returns u ^ v
pub fn (u Uint128) xor_64(v u64) Uint128 {
	return Uint128{u.lo ^ v, u.hi ^ 0}
}

// add returns u + v with wraparound semantics
pub fn (u Uint128) add(v Uint128) Uint128 {
	lo, carry := bits.add_64(u.lo, v.lo, 0)
	hi, _ := bits.add_64(u.hi, v.hi, carry)
	return Uint128{lo, hi}
}

// add_64 returns u + v with wraparound semantics
pub fn (u Uint128) add_64(v u64) Uint128 {
	lo, carry := bits.add_64(u.lo, v, 0)
	hi := u.hi + carry
	return Uint128{lo, hi}
}

// sub returns u - v with wraparound semantics
pub fn (u Uint128) sub(v Uint128) Uint128 {
	lo, borrow := bits.sub_64(u.lo, v.lo, 0)
	hi, _ := bits.sub_64(u.hi, v.hi, borrow)
	return Uint128{lo, hi}
}

// sub_64 returns u - v with wraparound semantics
pub fn (u Uint128) sub_64(v u64) Uint128 {
	lo, borrow := bits.sub_64(u.lo, v, 0)
	hi := u.hi - borrow
	return Uint128{lo, hi}
}

// mul returns u * v with wraparound semantics
pub fn (u Uint128) mul(v Uint128) Uint128 {
	mut hi, lo := bits.mul_64(u.lo, v.lo)
	hi += u.hi * v.lo + u.lo * v.hi
	return Uint128{lo, hi}
}

// mul_64 returns u * v with wraparound semantics
pub fn (u Uint128) mul_64(v u64) Uint128 {
	mut hi, lo := bits.mul_64(u.lo, v)
	hi += u.hi * v
	return Uint128{lo, hi}
}

// div returns u / v
pub fn (u Uint128) div(v Uint128) Uint128 {
	q, _ := u.quo_rem(v)
	return q
}

// mod returns r = u % v
pub fn (u Uint128) mod(v Uint128) Uint128 {
	_, r := u.quo_rem(v)
	return r
}

// mod_64 returns r = u % v
pub fn (u Uint128) mod_64(v u64) u64 {
	_, r := u.quo_rem_64(v)
	return r
}

// quo_rem_64 returns q = u/v and r = u%v
pub fn (u Uint128) quo_rem_64(v u64) (Uint128, u64) {
	if u.hi < v {
		mut r := u64(0)
		mut q := Uint128{0, 0}
		q.lo, r = bits.div_64(u.hi, u.lo, v)

		return q, r
	} else {
		mut q := Uint128{0, 0}
		mut r := u64(0)
		mut r2 := u64(0)
		q.hi, r = bits.div_64(0, u.hi, v)
		q.lo, r2 = bits.div_64(r, u.lo, v)
		return q, r2
	}
}

// quo_rem returns q = u/v and r = u%v
pub fn (u Uint128) quo_rem(v Uint128) (Uint128, Uint128) {
	mut q := Uint128{}
	mut r := Uint128{}
	if v.hi == 0 {
		mut r64 := u64(0)
		q, r64 = u.quo_rem_64(v.lo)
		r = Uint128{r64, 0}
	} else {
		n := bits.leading_zeros_64(v.hi)
		v1 := v.lsh(u32(n))
		u1 := v.rsh(1)

		mut tq, _ := bits.div_64(u1.hi, u1.lo, v1.hi)
		tq >>= u64(64 - n)
		if tq != 0 {
			tq -= 1
		}

		q = Uint128{tq, 0}
		r = u - v.mul_64(tq)

		if r.cmp(v) >= 0 {
			q = q.add_64(1)
			r = r - v
		}
	}

	return q, r
}

// lsh returns u << n
pub fn (u Uint128) lsh(n u32) Uint128 {
	mut s := Uint128{}
	if n > 64 {
		s.lo = 0
		s.hi = u.lo << (n - 64)
	} else {
		s.lo = u.lo << n
		s.hi = u.hi << n | u.lo >> (64 - n)
	}

	return s
}

// lsh returns u >> n
pub fn (u Uint128) rsh(n u32) Uint128 {
	mut s := Uint128{}
	if n > 64 {
		s.hi = 0
		s.lo = u.hi << (n - 64)
	} else {
		s.lo = u.lo >> n | u.hi << (64 - n)
		s.hi = u.hi >> n
	}

	return s
}

// leading_zeros returns the number of leading zero bits in u; the result is 128
// for u == 0.
pub fn (u Uint128) leading_zeros() int {
	if u.hi > 0 {
		return bits.leading_zeros_64(u.hi)
	}
	return 64 + bits.leading_zeros_64(u.lo)
}

// trailing_zeros returns the number of trailing zero bits in u; the result is
// 128 for u == 0.
pub fn (u Uint128) trailing_zeros() int {
	if u.lo > 0 {
		return bits.trailing_zeros_64(u.lo)
	}
	return 64 + bits.trailing_zeros_64(u.hi)
}

// ones_count returns the number of one bits ("population count" in u)
pub fn (u Uint128) ones_count() int {
	return bits.ones_count_64(u.hi) + bits.ones_count_64(u.lo)
}

// rotate_left returns the value of u rotated left by (k mod 128) bits.
pub fn (u Uint128) rotate_left(k int) Uint128 {
	n := u32(128)
	s := u32(k) & (n - 1)
	return u.lsh(s).or_(u.rsh(n - s))
}

// rotate_right returns the value of u rotated right by (k mod 128) bits.
pub fn (u Uint128) rotate_right(k int) Uint128 {
	return u.rotate_left(-k)
}

// reverse returns the value of u with its bits in reversed order.
pub fn (u Uint128) reverse() Uint128 {
	return Uint128{bits.reverse_64(u.hi), bits.reverse_64(u.lo)}
}

// reverse_bytes returns the value of u with its bytes in reversed order.
pub fn (u Uint128) reverse_bytes() Uint128 {
	return Uint128{bits.reverse_bytes_64(u.hi), bits.reverse_bytes_64(u.lo)}
}

// len returns the minimum number of bits required to represent u; the result is
// 0 for u == 0.
pub fn (u Uint128) len() int {
	return 128 - u.leading_zeros()
}

// string returns the base-10 representation of u as a string
pub fn (u_ Uint128) str() string {
	mut u := u_
	if u.is_zero() {
		return '0'
	}

	mut buf := '0000000000000000000000000000000000000000'.bytes() // log10(2^128) < 40

	for i := buf.len; true; i -= 19 {
		q, mut r := u.quo_rem_64(u64(1e19))

		mut n := int(0)
		for ; r != 0; r /= 10 {
			n++
			buf[i - n] += byte(r % 10)
		}
		if q.is_zero() {
			return buf[i - n..].bytestr()
		}
		u = q
	}

	return ''
}

pub fn uint128_new(lo u64, hi u64) Uint128 {
	return Uint128{lo, hi}
}

pub fn (u Uint128) / (v Uint128) Uint128 {
	return u.div(v)
}

pub fn (u Uint128) % (v Uint128) Uint128 {
	return u.mod(v)
}

pub fn (u Uint128) + (v Uint128) Uint128 {
	return u.add(v)
}

pub fn (u Uint128) - (v Uint128) Uint128 {
	return u.sub(v)
}

pub fn (u Uint128) * (v Uint128) Uint128 {
	return u.mul(v)
}
