module big

import math.bits

// help routines for cleaner code but inline for performance
// quicker than BitField.set_bit
[inline]
fn bit_set(mut a []u32, n int) {
	byte_offset := n / 32
	mask := u32(1) << u32(n % 32)
	assert a.len >= byte_offset
	a[byte_offset] |= mask
}

// a.len is greater or equal to b.len
// returns true if a >= b (completed with zeroes)
[inline]
fn greater_equal_from_end(a []u32, b []u32) bool {
	assert a.len >= b.len
	offset := a.len - b.len
	for index := a.len - 1; index >= offset; index-- {
		if a[index] > b[index - offset] {
			return true
		} else if a[index] < b[index - offset] {
			return false
		}
	}
	return true
}

// logical left shift
// there is no overflow. We know that the last bits are zero
// and that n <= 32
[inline]
fn lshift_in_place(mut a []u32, n u32) {
	mut carry := u32(0)
	mut prec_carry := u32(0)
	mask := ((u32(1) << n) - 1) << (32 - n)
	for index in 0 .. a.len {
		prec_carry = carry >> (32 - n)
		carry = a[index] & mask
		a[index] <<= n
		a[index] |= prec_carry
	}
}

// logical right shift without control because these digits have already been
// shift left before
[inline]
fn rshift_in_place(mut a []u32, n u32) {
	mut carry := u32(0)
	mut prec_carry := u32(0)
	mask := u32((1 << n) - 1)
	for index := a.len - 1; index >= 0; index-- {
		carry = a[index] & mask
		a[index] >>= n
		a[index] |= prec_carry << (32 - n)
		prec_carry = carry
	}
}

// for assert
[inline]
fn left_align_p(a u32, b u32) bool {
	return bits.leading_zeros_32(a) == bits.leading_zeros_32(b)
}
