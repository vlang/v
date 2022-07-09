module unsigned

import math.bits

pub const (
	uint256_zero = Uint256{Uint128{}, Uint128{}}
	uint256_max  = Uint256{uint128_max, uint128_max}
)

// Uint256 is an unsigned 256-bit number
pub struct Uint256 {
pub mut:
	lo Uint128 = uint128_zero // lower 128 bit half
	hi Uint128 = uint128_zero // upper 128 bit half
}

// uint256_from_128 creates a new `unsigned.Uint256` from the given Uint128 value
pub fn uint256_from_128(v Uint128) Uint256 {
	return Uint256{v, uint128_zero}
}

// uint256_from_64 creates a new `unsigned.Uint256` from the given u64 value
pub fn uint256_from_64(v u64) Uint256 {
	return uint256_from_128(uint128_from_64(v))
}

// is_zero checks if specified Uint256 is zero
pub fn (u Uint256) is_zero() bool {
	return u.lo.is_zero() && u.hi.is_zero()
}

// equals checks if the two Uint256 values match one another
pub fn (u Uint256) equals(v Uint256) bool {
	return u.lo.equals(v.lo) && u.hi.equals(v.hi)
}

// equals_128 checks if the Uint256 value matches the Uint128 value
pub fn (u Uint256) equals_128(v Uint128) bool {
	return u.lo.equals(v) && u.hi.is_zero()
}

// cmp returns 1 if u is greater than v, -1 if u is less than v, or 0 if equal
pub fn (u Uint256) cmp(v Uint256) int {
	h := u.hi.cmp(v.hi)
	if h != 0 {
		return h
	}
	return u.lo.cmp(v.lo)
}

// cmp_128 returns 1 if u is greater than v (Uint128), -1 if u is less than v, or 0 if equal
pub fn (u Uint256) cmp_128(v Uint128) int {
	if !u.hi.is_zero() {
		return 1
	}
	return u.lo.cmp(v)
}

// not returns a binary negation of the Uint256 value
pub fn (u Uint256) not() Uint256 {
	return Uint256{u.lo.not(), u.hi.not()}
}

// and returns a Uint256 value that is the bitwise and of u and v
pub fn (u Uint256) and(v Uint256) Uint256 {
	return Uint256{u.lo.and(v.lo), u.hi.and(v.hi)}
}

// and_128 returns a Uint256 value that is the bitwise and of u and v, which is a Uint128
pub fn (u Uint256) and_128(v Uint128) Uint256 {
	return Uint256{u.lo.and(v), uint128_zero}
}

// or_ returns a Uint256 value that is the bitwise or of u and v
pub fn (u Uint256) or_(v Uint256) Uint256 {
	return Uint256{u.lo.or_(v.lo), u.hi.or_(v.hi)}
}

// or_128 returns a Uint256 value that is the bitwise or of u and v, which is a Uint128
pub fn (u Uint256) or_128(v Uint128) Uint256 {
	return Uint256{u.lo.or_(v), u.hi}
}

// xor returns a Uint256 value that is the bitwise xor of u and v
pub fn (u Uint256) xor(v Uint256) Uint256 {
	return Uint256{u.lo.xor(v.lo), u.hi.xor(v.hi)}
}

// xor_128 returns a Uint256 value that is the bitwise xor of u and v, which is a Uint128
pub fn (u Uint256) xor_128(v Uint128) Uint256 {
	return Uint256{u.lo.xor(v), u.hi}
}

// add_256 - untested
pub fn add_256(x Uint256, y Uint256, carry u64) (Uint256, u64) {
	mut sum := Uint256{}
	mut carry_out := u64(0)
	sum.lo, carry_out = add_128(x.lo, y.lo, carry)
	sum.hi, carry_out = add_128(x.hi, y.hi, carry_out)
	return sum, carry_out
}

// sub_256 - untested
pub fn sub_256(x Uint256, y Uint256, borrow u64) (Uint256, u64) {
	mut diff := Uint256{}
	mut borrow_out := u64(0)
	diff.lo, borrow_out = sub_128(x.lo, y.lo, borrow)
	diff.hi, borrow_out = sub_128(x.hi, y.hi, borrow_out)
	return diff, borrow_out
}

// mul_256 - untested
pub fn mul_256(x Uint256, y Uint256) (Uint256, Uint256) {
	mut hi := Uint256{}
	mut lo := Uint256{}

	lo.hi, lo.lo = mul_128(x.lo, y.lo)
	hi.hi, hi.lo = mul_128(x.hi, y.hi)
	t0, t1 := mul_128(x.lo, y.hi)
	t2, t3 := mul_128(x.hi, y.lo)

	mut c0 := u64(0)
	mut c1 := u64(0)

	lo.hi, c0 = add_128(lo.hi, t1, 0)
	lo.hi, c1 = add_128(lo.hi, t3, 0)
	hi.lo, c0 = add_128(hi.lo, t0, c0)
	hi.lo, c1 = add_128(hi.lo, t2, c1)
	hi.hi = hi.hi.add_64(c0 + c1)

	return hi, lo
}

// add returns a Uint256 that is equal to u+v
pub fn (u Uint256) add(v Uint256) Uint256 {
	sum, _ := add_256(u, v, 0)
	return sum
}

// overflowing_add - untested
pub fn (u Uint256) overflowing_add(v Uint256) (Uint256, u64) {
	sum, overflow := add_256(u, v, 0)
	return sum, overflow
}

// add_128 returns a Uint256 that is equal to u+v, v being a Uint128
pub fn (u Uint256) add_128(v Uint128) Uint256 {
	lo, c0 := add_128(u.lo, v, 0)
	return Uint256{lo, u.hi.add_64(c0)}
}

// sub returns a Uint256 that is equal to u-v
pub fn (u Uint256) sub(v Uint256) Uint256 {
	diff, _ := sub_256(u, v, 0)
	return diff
}

// sub_128 returns a Uint256 that is equal to u-v, v being a Uint128
pub fn (u Uint256) sub_128(v Uint128) Uint256 {
	lo, b0 := sub_128(u.lo, v, 0)
	return Uint256{lo, u.hi.sub_64(b0)}
}

// mul returns a Uint256 that is eqal to u*v
pub fn (u Uint256) mul(v Uint256) Uint256 {
	mut hi, mut lo := mul_128(u.lo, v.lo)
	hi = hi.add(u.hi.mul(v.lo))
	hi = hi.add(u.lo.mul(v.hi))
	return Uint256{lo, hi}
}

// mul_128 returns a Uint256 that is eqal to u*v, v being a Uint128
pub fn (u Uint256) mul_128(v Uint128) Uint256 {
	hi, lo := mul_128(u.lo, v)
	return Uint256{lo, hi.add(u.hi.mul(v))}
}

// quo_rem - untested
pub fn (u Uint256) quo_rem(v Uint256) (Uint256, Uint256) {
	if v.hi.is_zero() {
		q, r := u.quo_rem_128(v.lo)
		return q, uint256_from_128(r)
	}

	n := u64(v.hi.leading_zeros())
	u1, v1 := u.rsh(1), v.lsh(u32(n))
	mut tq, _ := div_128(u1.hi, u1.lo, v1.hi)
	tq = tq.rsh(u32(127 - n))
	if !tq.is_zero() {
		tq = tq.sub_64(1)
	}

	mut q, mut r := uint256_from_128(tq), u.sub(v.mul_128(tq))
	if r.cmp(v) >= 0 {
		q = q.add_128(Uint128{1, 0})
		r = r.sub(v)
	}
	return q, r
}

// quo_rem_128 - untested
pub fn (u Uint256) quo_rem_128(v Uint128) (Uint256, Uint128) {
	if u.hi.cmp(v) < 0 {
		lo, r := div_128(u.hi, u.lo, v)
		return Uint256{lo, uint128_zero}, r
	}

	hi, r := div_128(uint128_zero, u.hi, v)
	lo, r2 := div_128(r, u.lo, v)
	return Uint256{lo, hi}, r2
}

// quo_rem_64 - untested
pub fn (u Uint256) quo_rem_64(v u64) (Uint256, u64) {
	mut q := Uint256{}
	mut r := u64(0)
	q.lo.hi, r = bits.div_64(r, u.lo.hi, v)
	q.lo.lo, r = bits.div_64(r, u.lo.lo, v)
	return q, r
}

// rsh returns a new Uint256 that has been right bit shifted
pub fn (u Uint256) rsh(n_ u32) Uint256 {
	mut n := n_
	if n > 128 {
		return Uint256{u.hi.rsh(n - 128), uint128_zero}
	}

	if n > 64 {
		n -= 64
		return Uint256{Uint128{u.lo.hi >> n | u.hi.lo << (64 - n), u.hi.lo >> n | u.hi.hi << (64 - n)}, Uint128{u.hi.hi >> n, 0}}
	}
	return Uint256{Uint128{u.lo.lo >> n | u.lo.hi << (64 - n), u.lo.hi >> n | u.hi.lo << (64 - n)}, Uint128{u.hi.lo >> n | u.hi.hi << (64 - n), u.hi.hi >> n}}
}

// lsh returns a new Uint256 that has been left bit shifted
pub fn (u Uint256) lsh(n_ u32) Uint256 {
	mut n := n_
	if n > 128 {
		return Uint256{u.lo.lsh(n - 128), uint128_zero}
	}

	if n > 64 {
		n -= 64
		return Uint256{Uint128{u.lo.lo << n, 0}, Uint128{u.lo.hi << n | u.lo.lo >> (64 - n), u.hi.lo << n | u.lo.hi >> (64 - n)}}
	}

	return Uint256{Uint128{u.lo.lo << n, u.lo.hi << n | u.lo.lo >> (64 - n)}, Uint128{u.hi.lo << n | u.lo.hi >> (64 - n), u.hi.hi << n | u.hi.lo >> (64 - n)}}
}

// div - untested
pub fn (u Uint256) div(v Uint256) Uint256 {
	q, _ := u.quo_rem(v)
	return q
}

// div_128 - untested
pub fn (u Uint256) div_128(v Uint128) Uint256 {
	q, _ := u.quo_rem_128(v)
	return q
}

// div_64 - untested
pub fn (u Uint256) div_64(v u64) Uint256 {
	q, _ := u.quo_rem_64(v)
	return q
}

// mod - untested
pub fn (u Uint256) mod(v Uint256) Uint256 {
	_, r := u.quo_rem(v)
	return r
}

// mod_128 - untested
pub fn (u Uint256) mod_128(v Uint128) Uint128 {
	_, r := u.quo_rem_128(v)
	return r
}

// mod_64 - untested
pub fn (u Uint256) mod_64(v u64) u64 {
	_, r := u.quo_rem_64(v)
	return r
}

// rotate_left returns a new Uint256 that has been left bit shifted
pub fn (u Uint256) rotate_left(k int) Uint256 {
	mut n := u32(k) & 255
	if n < 64 {
		if n == 0 {
			return u
		}

		return Uint256{Uint128{u.lo.lo << n | u.hi.hi >> (64 - n), u.lo.hi << n | u.lo.lo >> (64 - n)}, Uint128{u.hi.lo << n | u.lo.hi >> (64 - n), u.hi.hi << n | u.hi.lo >> (64 - n)}}
	}
	n -= 64
	if n < 64 {
		if n == 0 {
			return Uint256{Uint128{u.hi.hi, u.lo.lo}, Uint128{u.lo.hi, u.hi.lo}}
		}

		return Uint256{Uint128{u.hi.hi << n | u.hi.lo >> (64 - n), u.lo.lo << n | u.hi.hi >> (64 - n)}, Uint128{u.lo.hi << n | u.lo.lo >> (64 - n), u.hi.lo << n | u.lo.hi >> (64 - n)}}
	}

	n -= 64
	if n < 64 {
		if n == 0 {
			return Uint256{u.hi, u.lo}
		}

		return Uint256{Uint128{u.hi.lo << n | u.lo.hi >> (64 - n), u.hi.hi << n | u.hi.lo >> (64 - n)}, Uint128{}}
	}
	n -= 64
	if n == 0 {
		return Uint256{Uint128{u.lo.hi, u.hi.lo}, Uint128{u.hi.hi, u.lo.lo}}
	}

	return Uint256{Uint128{u.lo.hi << n | u.lo.lo >> (64 - n), u.hi.lo << n | u.lo.hi >> (64 - n)}, Uint128{u.hi.hi << n | u.hi.lo >> (64 - n), u.lo.lo << n | u.hi.hi >> (64 - n)}}
}

// rotate_right returns a new Uint256 that has been right bit shifted
pub fn (u Uint256) rotate_right(k int) Uint256 {
	return u.rotate_left(-k)
}

// len returns the length of the binary value without the leading zeros
pub fn (u Uint256) len() int {
	if !u.hi.is_zero() {
		return 128 + u.hi.len()
	}
	return u.lo.len()
}

// leading_zeros returns the number of 0s at the beginning of the binary value of the Uint256 value [0, 256]
pub fn (u Uint256) leading_zeros() int {
	if !u.hi.is_zero() {
		return u.hi.leading_zeros()
	}
	return 128 + u.lo.leading_zeros()
}

// trailing_zeros returns the number of 0s at the end of the binary value of the Uint256 value [0,256]
pub fn (u Uint256) trailing_zeros() int {
	if !u.lo.is_zero() {
		return u.lo.trailing_zeros()
	}

	return 128 + u.hi.trailing_zeros()
}

// ones_count returns the number of ones in the binary value of the Uint256 value
pub fn (u Uint256) ones_count() int {
	return u.lo.ones_count() + u.hi.ones_count()
}

// str returns the decimal representation of the unsigned integer
pub fn (u_ Uint256) str() string {
	mut u := u_
	if u.hi.is_zero() {
		if u.lo.is_zero() {
			return '0'
		}
		return u.lo.str()
	}

	mut buf := '000000000000000000000000000000000000000000000000000000000000000000000000000000'.bytes()

	for i := buf.len; true; i -= 19 {
		q, mut r := u.quo_rem_64(u64(1e19))
		mut n := 0
		for ; r != 0; r /= 10 {
			n++
			buf[i - n] += u8(r % 10)
		}
		if q.is_zero() {
			return buf[i - n..].bytestr()
		}
		u = q
	}
	return ''
}

// uint256_from_dec_str creates a new `unsigned.Uint256` from the given string if possible
pub fn uint256_from_dec_str(value string) ?Uint256 {
	mut res := unsigned.uint256_zero
	for b_ in value.bytes() {
		b := b_ - '0'.bytes()[0]
		if b > 9 {
			return error('invalid character "$b"')
		}

		r := res.mul_128(uint128_from_64(10))

		r2 := r.add_128(uint128_from_64(u64(b)))
		res = r2
	}
	return res
}

pub fn (u Uint256) / (v Uint256) Uint256 {
	return u.div(v)
}

pub fn (u Uint256) % (v Uint256) Uint256 {
	return u.mod(v)
}

pub fn (u Uint256) + (v Uint256) Uint256 {
	return u.add(v)
}

pub fn (u Uint256) - (v Uint256) Uint256 {
	return u.sub(v)
}

pub fn (u Uint256) * (v Uint256) Uint256 {
	return u.mul(v)
}
