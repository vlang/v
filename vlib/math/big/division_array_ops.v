module big

import math.bits

// suppose operand_a bigger than operand_b and both not null.
// Both quotient and remaider are allocated but of length 0
@[direct_array_access]
fn binary_divide_array_by_array(operand_a []u64, operand_b []u64, mut quotient []u64, mut remainder []u64) {
	remainder << operand_a

	len_diff := operand_a.len - operand_b.len
	$if debug {
		assert len_diff >= 0
	}

	// we must do in place shift and operations.
	mut divisor := []u64{cap: operand_b.len}
	for _ in 0 .. len_diff {
		divisor << u64(0)
	}
	divisor << operand_b
	for _ in 0 .. len_diff + 1 {
		quotient << u64(0)
	}

	lead_zer_remainder := u32(bits.leading_zeros_64(remainder.last()) - (64 - digit_bits))
	lead_zer_divisor := u32(bits.leading_zeros_64(divisor.last()) - (64 - digit_bits))
	bit_offset := (u32(digit_bits) * u32(len_diff)) + (lead_zer_divisor - lead_zer_remainder)

	// align
	if lead_zer_remainder < lead_zer_divisor {
		left_shift_in_place(mut divisor, lead_zer_divisor - lead_zer_remainder)
	} else if lead_zer_remainder > lead_zer_divisor {
		left_shift_in_place(mut remainder, lead_zer_remainder - lead_zer_divisor)
	}

	$if debug {
		assert left_align_p(divisor[divisor.len - 1], remainder[remainder.len - 1])
	}
	for bit_idx := int(bit_offset); bit_idx >= 0; bit_idx-- {
		if greater_equal_from_end(remainder, divisor) {
			bit_set(mut quotient, bit_idx)
			subtract_align_last_byte_in_place(mut remainder, divisor)
		}
		right_shift_in_place(mut divisor, 1)
	}

	// adjust
	if lead_zer_remainder > lead_zer_divisor {
		right_shift_in_place(mut remainder, lead_zer_remainder - lead_zer_divisor)
	}
	shrink_tail_zeros(mut remainder)
	shrink_tail_zeros(mut quotient)
}

// help routines for cleaner code but inline for performance
// quicker than BitField.set_bit
@[direct_array_access; inline]
fn bit_set(mut a []u64, n int) {
	byte_offset := n / digit_bits
	mask := u64(1) << u32(n % digit_bits)
	$if debug {
		assert a.len >= byte_offset
	}
	a[byte_offset] |= mask
}

// a.len is greater or equal to b.len
// returns true if a >= b (completed with zeroes)
@[direct_array_access; inline]
fn greater_equal_from_end(a []u64, b []u64) bool {
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
@[direct_array_access; inline]
fn subtract_align_last_byte_in_place(mut a []u64, b []u64) {
	mut carry := u64(0)
	mut new_carry := u64(0)
	offset := a.len - b.len
	for index := a.len - b.len; index < a.len; index++ {
		if a[index] < (b[index - offset] + carry) || (b[index - offset] == max_digit && carry > 0) {
			new_carry = 1
		} else {
			new_carry = 0
		}
		a[index] -= (b[index - offset] + carry)
		a[index] = a[index] & max_digit
		carry = new_carry
	}
	$if debug {
		assert carry == 0
	}
}

// logical left shift
// there is no overflow. We know that the last bits are zero
// and that n <= `digit_bits`
@[direct_array_access; inline]
fn left_shift_in_place(mut a []u64, n u32) {
	mut carry := u64(0)
	mut prec_carry := u64(0)
	mask := ((u64(1) << n) - 1) << (digit_bits - n)
	for index in 0 .. a.len {
		prec_carry = carry >> (digit_bits - n)
		carry = a[index] & mask
		a[index] <<= n
		a[index] = a[index] & max_digit
		a[index] |= prec_carry
	}
}

// logical right shift without control because these digits have already been
// shift left before
@[direct_array_access; inline]
fn right_shift_in_place(mut a []u64, n u32) {
	mut carry := u64(0)
	mut prec_carry := u64(0)
	mask := (u64(1) << n) - 1
	for index := a.len - 1; index >= 0; index-- {
		carry = a[index] & mask
		a[index] >>= n
		a[index] |= prec_carry << (digit_bits - n)
		prec_carry = carry
	}
}

// for assert
@[inline]
fn left_align_p(a u64, b u64) bool {
	return bits.leading_zeros_64(a) == bits.leading_zeros_64(b)
}
