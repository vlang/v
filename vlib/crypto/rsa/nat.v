module rsa

import math.bits

// This file implements the fixed-width modular arithmetic behind RSA, on
// little-endian arrays of 64-bit limbs. Every operation that handles secret
// values runs in time that depends only on the sizes of its operands: it never
// branches on, or indexes memory with, the values themselves. Choices are made
// with masks instead (see `ct_select`). The design follows Go's
// crypto/internal/fips140/bigmod.

// Modulus is an odd modulus `n` with its Montgomery constants: `m0inv` is
// -n^-1 mod 2^64, and `rr` is R^2 mod n, with R = 2^(64 * limbs).
struct Modulus {
	n     []u64
	m0inv u64
	rr    []u64
	bits  int
}

// ct_mask returns all ones when `bit` is 1, and zero when it is 0. Every
// secret choice goes through it. The mask passes through `value_barrier`, so
// that the C compiler cannot tell it is 0 or all ones, and turn the masked
// selection that uses it back into a branch.
@[inline]
fn ct_mask(bit u64) u64 {
	return value_barrier(u64(0) - bit)
}

// value_barrier returns `x`, through an empty assembly statement that the C
// compiler must assume reads and changes it. Without inline assembly (TCC,
// other architectures), it returns `x` unchanged.
@[inline]
fn value_barrier(x u64) u64 {
	mut r := x
	$if amd64 && !tinyc {
		asm volatile amd64 {
			; +r (r)
		}
	} $else $if arm64 && !tinyc {
		asm volatile arm64 {
			; +r (r)
		}
	}
	return r
}

// ct_is_zero returns 1 when `x` is zero, and 0 otherwise.
@[inline]
fn ct_is_zero(x u64) u64 {
	return ((x | (u64(0) - x)) >> 63) ^ 1
}

// ct_select sets `dst` to `a` when `bit` is 1, and to `b` when it is 0.
@[direct_array_access]
fn ct_select(mut dst []u64, bit u64, a []u64, b []u64) {
	mask := ct_mask(bit)
	for i in 0 .. dst.len {
		dst[i] = (a[i] & mask) | (b[i] & ~mask)
	}
}

// limbs_for returns how many 64-bit limbs hold a `bit_len`-bit number.
fn limbs_for(bit_len int) int {
	return (bit_len + 63) / 64
}

// nat_from_bytes reads the big-endian `b` into `limbs` limbs. The caller
// guarantees that the value fits.
fn nat_from_bytes(b []u8, limbs int) []u64 {
	mut out := []u64{len: limbs}
	for i in 0 .. b.len {
		pos := b.len - 1 - i
		out[i / 8] |= u64(b[pos]) << (8 * u32(i % 8))
	}
	return out
}

// nat_to_bytes writes `x` as a big-endian number of exactly `size` bytes. The
// caller guarantees that the value fits.
fn nat_to_bytes(x []u64, size int) []u8 {
	mut out := []u8{len: size}
	for i in 0 .. size {
		limb := i / 8
		if limb < x.len {
			out[size - 1 - i] = u8(x[limb] >> (8 * u32(i % 8)))
		}
	}
	return out
}

// nat_sub sets `x` to `x - y` and returns the borrow.
@[direct_array_access]
fn nat_sub(mut x []u64, y []u64) u64 {
	mut borrow := u64(0)
	for i in 0 .. x.len {
		x[i], borrow = bits.sub_64(x[i], y[i], borrow)
	}
	return borrow
}

// nat_add sets `x` to `x + y` and returns the carry.
@[direct_array_access]
fn nat_add(mut x []u64, y []u64) u64 {
	mut carry := u64(0)
	for i in 0 .. x.len {
		x[i], carry = bits.add_64(x[i], y[i], carry)
	}
	return carry
}

// nat_less returns 1 when `x < y`, and 0 otherwise. Both have the same length.
@[direct_array_access]
fn nat_less(x []u64, y []u64) u64 {
	mut borrow := u64(0)
	for i in 0 .. x.len {
		_, borrow = bits.sub_64(x[i], y[i], borrow)
	}
	return borrow
}

// reduce_once subtracts `m.n` from `x` when `extra` is set or `x >= m.n`. It
// brings back below the modulus any value smaller than 2 * m.n, where `extra`
// is the bit above the top limb of `x`. `scratch` has the length of `x`.
@[direct_array_access]
fn (m &Modulus) reduce_once(mut x []u64, extra u64, mut scratch []u64) {
	mut borrow := u64(0)
	for i in 0 .. x.len {
		scratch[i], borrow = bits.sub_64(x[i], m.n[i], borrow)
	}
	// Keep the difference when it did not underflow, or when the true value
	// had a bit above the top limb.
	mask := ct_mask(extra | (borrow ^ 1))
	for i in 0 .. x.len {
		x[i] = (scratch[i] & mask) | (x[i] & ~mask)
	}
}

// new_modulus returns the Modulus for the big-endian `n`, which must be odd and
// greater than one. `n` is public for an RSA modulus, and secret for a prime
// factor: nothing here depends on its value, only on its length.
fn new_modulus(n []u8) !Modulus {
	mut start := 0
	for start < n.len && n[start] == 0 {
		start++
	}
	trimmed := n[start..]
	if trimmed.len == 0 || trimmed[trimmed.len - 1] & 1 == 0 {
		return error('crypto.rsa: modulus must be odd')
	}
	bit_len := (trimmed.len - 1) * 8 + bits.len_8(trimmed[0])
	if bit_len < 2 {
		return error('crypto.rsa: modulus must be greater than one')
	}
	limbs := limbs_for(bit_len)
	nn := nat_from_bytes(trimmed, limbs)
	// Newton's iteration doubles the number of correct low bits of n^-1 mod
	// 2^64 at each step; n0 is odd, so 1 is correct for the lowest bit.
	mut inv := u64(1)
	for _ in 0 .. 6 {
		inv *= 2 - nn[0] * inv
	}
	mut m := Modulus{
		n:     nn
		m0inv: u64(0) - inv
		bits:  bit_len
	}
	// R^2 mod n, from the number 2^(2 * 64 * limbs).
	mut r2 := []u64{len: 2 * limbs + 1}
	r2[2 * limbs] = 1
	rr := m.reduce(r2)
	return Modulus{
		...m
		rr: rr
	}
}

// size returns the length of the modulus in bytes.
fn (m &Modulus) size() int {
	return (m.bits + 7) / 8
}

// limbs returns the length of the modulus in limbs.
fn (m &Modulus) limbs() int {
	return m.n.len
}

// reduce returns `x mod m` for an `x` of any length. It shifts `x` into the
// result one bit at a time, from the top, which keeps it in constant time.
@[direct_array_access]
fn (m &Modulus) reduce(x []u64) []u64 {
	limbs := m.limbs()
	mut out := []u64{len: limbs}
	mut scratch := []u64{len: limbs}
	for i := x.len - 1; i >= 0; i-- {
		for b := 63; b >= 0; b-- {
			// out = 2 * out + bit, keeping the bit shifted out of the top.
			mut carry := (x[i] >> u32(b)) & 1
			for j in 0 .. limbs {
				next := out[j] >> 63
				out[j] = (out[j] << 1) | carry
				carry = next
			}
			m.reduce_once(mut out, carry, mut scratch)
		}
	}
	return out
}

// mont_mul returns `a * b * R^-1 mod m`, for `a` and `b` below the modulus.
fn (m &Modulus) mont_mul(a []u64, b []u64) []u64 {
	k := m.limbs()
	mut out := []u64{len: k}
	mut t := []u64{len: k + 2}
	m.mont_mul_to(mut out, a, b, mut t)
	return out
}

// mont_mul_to sets `out` to `a * b * R^-1 mod m`, for `a` and `b` below the
// modulus, with the scratch space `t` of `limbs() + 2` limbs. `out` may be
// `a` or `b`.
@[direct_array_access]
fn (m &Modulus) mont_mul_to(mut out []u64, a []u64, b []u64, mut t []u64) {
	k := m.limbs()
	for i in 0 .. t.len {
		t[i] = 0
	}
	for i in 0 .. k {
		// t += a * b[i]
		bi := b[i]
		mut c := u64(0)
		mut carry := u64(0)
		for j in 0 .. k {
			mut hi, mut lo := bits.mul_64(a[j], bi)
			lo, carry = bits.add_64(lo, t[j], 0)
			hi += carry
			lo, carry = bits.add_64(lo, c, 0)
			hi += carry
			t[j] = lo
			c = hi
		}
		t[k], carry = bits.add_64(t[k], c, 0)
		t[k + 1] = carry
		// t = (t + q * n) / 2^64, with q chosen to clear the low limb.
		q := t[0] * m.m0inv
		mut hi, mut lo := bits.mul_64(q, m.n[0])
		_, carry = bits.add_64(lo, t[0], 0)
		c = hi + carry
		for j in 1 .. k {
			hi, lo = bits.mul_64(q, m.n[j])
			lo, carry = bits.add_64(lo, t[j], 0)
			hi += carry
			lo, carry = bits.add_64(lo, c, 0)
			hi += carry
			t[j - 1] = lo
			c = hi
		}
		t[k - 1], carry = bits.add_64(t[k], c, 0)
		t[k] = t[k + 1] + carry
	}
	for i in 0 .. k {
		out[i] = t[i]
	}
	m.reduce_once(mut out, t[k], mut t)
}

// exp returns `x^e mod m` for `x` below the modulus, with the big-endian
// exponent `e`. It uses a fixed 4-bit window and reads the precomputed powers
// with `ct_lookup`, so its timing depends only on the length of `e`.
fn (m &Modulus) exp(x []u64, e []u8) []u64 {
	k := m.limbs()
	mut scratch := []u64{len: k + 2}
	mut one := []u64{len: k}
	one[0] = 1
	mut table := [][]u64{cap: 16}
	for _ in 0 .. 16 {
		table << []u64{len: k}
	}
	m.mont_mul_to(mut table[0], one, m.rr, mut scratch) // R mod m, the Montgomery form of 1
	m.mont_mul_to(mut table[1], x, m.rr, mut scratch)
	for i in 2 .. 16 {
		m.mont_mul_to(mut table[i], table[i - 1], table[1], mut scratch)
	}
	mut out := table[0].clone()
	mut t := []u64{len: k}
	for b in e {
		for nibble in [u64(b >> 4), u64(b & 0x0f)] {
			for _ in 0 .. 4 {
				m.mont_mul_to(mut out, out, out, mut scratch)
			}
			ct_lookup(mut t, table, nibble)
			m.mont_mul_to(mut out, out, t, mut scratch)
		}
	}
	m.mont_mul_to(mut out, out, one, mut scratch)
	return out
}

// ct_lookup copies `table[index]` into `dst`, reading every entry.
@[direct_array_access]
fn ct_lookup(mut dst []u64, table [][]u64, index u64) {
	for i in 0 .. dst.len {
		dst[i] = 0
	}
	for i, entry in table {
		mask := ct_mask(ct_is_zero(u64(i) ^ index))
		for j in 0 .. dst.len {
			dst[j] |= entry[j] & mask
		}
	}
}

// nat_mul returns the full product `x * y`, with `x.len + y.len` limbs.
@[direct_array_access]
fn nat_mul(x []u64, y []u64) []u64 {
	mut out := []u64{len: x.len + y.len}
	for i in 0 .. y.len {
		mut c := u64(0)
		for j in 0 .. x.len {
			mut hi, mut lo := bits.mul_64(x[j], y[i])
			mut carry := u64(0)
			lo, carry = bits.add_64(lo, out[i + j], 0)
			hi += carry
			lo, carry = bits.add_64(lo, c, 0)
			hi += carry
			out[i + j] = lo
			c = hi
		}
		out[i + x.len] = c
	}
	return out
}
