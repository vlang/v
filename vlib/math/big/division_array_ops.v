module big

import math.bits

// suppose operand_a bigger than operand_b and both not null.
// Both quotient and remaider are allocated but of length 0
fn binary_divide_array_by_array(operand_a []u32, operand_b []u32, mut quotient []u32, mut remainder []u32) {
	for index in 0 .. operand_a.len {
		remainder << operand_a[index]
	}

	len_diff := operand_a.len - operand_b.len
	$if debug {
		assert len_diff >= 0
	}

	// we must do in place shift and operations.
	mut divisor := []u32{cap: operand_b.len}
	for _ in 0 .. len_diff {
		divisor << u32(0)
	}
	for index in 0 .. operand_b.len {
		divisor << operand_b[index]
	}
	for _ in 0 .. len_diff + 1 {
		quotient << u32(0)
	}

	lead_zer_remainder := u32(bits.leading_zeros_32(remainder.last()))
	lead_zer_divisor := u32(bits.leading_zeros_32(divisor.last()))
	bit_offset := (u32(32) * u32(len_diff)) + (lead_zer_divisor - lead_zer_remainder)

	// align
	if lead_zer_remainder < lead_zer_divisor {
		lshift_in_place(mut divisor, lead_zer_divisor - lead_zer_remainder)
	} else if lead_zer_remainder > lead_zer_divisor {
		lshift_in_place(mut remainder, lead_zer_remainder - lead_zer_divisor)
	}

	$if debug {
		assert left_align_p(divisor[divisor.len - 1], remainder[remainder.len - 1])
	}
	for bit_idx := int(bit_offset); bit_idx >= 0; bit_idx-- {
		if greater_equal_from_end(remainder, divisor) {
			bit_set(mut quotient, bit_idx)
			subtract_align_last_byte_in_place(mut remainder, divisor)
		}
		rshift_in_place(mut divisor, 1)
	}

	// adjust
	if lead_zer_remainder > lead_zer_divisor {
		// rshift_in_place(mut quotient, lead_zer_remainder - lead_zer_divisor)
		rshift_in_place(mut remainder, lead_zer_remainder - lead_zer_divisor)
	}
	for remainder.len > 0 && remainder.last() == 0 {
		remainder.delete_last()
	}
	for quotient.len > 0 && quotient.last() == 0 {
		quotient.delete_last()
	}
}

// help routines for cleaner code but inline for performance
// quicker than BitField.set_bit
[inline]
fn bit_set(mut a []u32, n int) {
	byte_offset := n >> 5
	mask := u32(1) << u32(n % 32)
	$if debug {
		assert a.len >= byte_offset
	}
	a[byte_offset] |= mask
}

// a.len is greater or equal to b.len
// returns true if a >= b (completed with zeroes)
[inline]
fn greater_equal_from_end(a []u32, b []u32) bool {
	$if debug {
		assert a.len >= b.len
	}
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

// a := a - b supposed a >= b
// attention the b operand is align with the a operand before the subtraction
[inline]
fn subtract_align_last_byte_in_place(mut a []u32, b []u32) {
	mut carry := u32(0)
	mut new_carry := u32(0)
	offset := a.len - b.len
	for index := a.len - b.len; index < a.len; index++ {
		if a[index] < (b[index - offset] + carry) {
			new_carry = 1
		} else {
			new_carry = 0
		}
		a[index] -= (b[index - offset] + carry)
		carry = new_carry
	}
	$if debug {
		assert carry == 0
	}
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
