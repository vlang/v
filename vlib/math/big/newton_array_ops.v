module big

import math.bits
import math.util

// import math.bits

// suppose operand_a bigger than operand_b and both not null.
// Both quotient and remaider are allocated but of length 0
fn divide_array_by_array(operand_a []u32, operand_b []u32, mut quotient []u32, mut remainder []u32) {

	for _ in 0 .. operand_a.len {
		remainder << u32(0)
	}

	len_diff := operand_a.len - operand_b.len
	assert len_diff >= 0

	// estimate bit length of result and build it
	operand_a_bit_len := operand_a.len * 32 - bits.leading_zeros_32(operand_a.last())
	operand_b_bit_len := operand_b.len * 32 - bits.leading_zeros_32(operand_b.last())
	quotient_bit_len := operand_a_bit_len - operand_b_bit_len + 1
	assert quotient_bit_len >= 0

	for i in 0 .. (quotient_bit_len / 32) + 1 {
		quotient << operand_a[i]
	}

	mask := (u32(1) << quotient_bit_len % 32) - 1
	quotient[quotient.len - 1] = quotient.last() & mask

	// initialize arrays outside of the loop
	mut product := []u32{len: operand_a.len}
	mut derivate := []u32{len: operand_a.len - operand_b.len + 1, init: 0}
	mut save_difference := []u32{len: operand_a.len, init: 0}
	mut slope := []u32{cap: operand_a.len}
	mut step := 0
loop:
	// tentative multiplication and difference
	// clear first the product
	for i in 0 .. product.len {
		product[i] = u32(0)
	}
	for _ in product.len .. operand_a.len { // and reinitialize to the length
		product << u32(0)
	}
	multiply_digit_array(operand_b, quotient, mut product)
	cmp := compare_digit_array(operand_a, product)
	if cmp == 0 {
		unsafe {goto null_remainder}
	} else if cmp < 0 {
		for i in 0 .. remainder.len { // remainder can be change by divide_digit_array to calculate slope
			remainder[i] = u32(0)
		}
		for _ in remainder.len .. operand_a.len {
			remainder << u32(0)
		}
		subtract_digit_array(product, operand_a, mut remainder)
		if compare_digit_array(remainder, operand_b) < 0 {
			// remainder is negative: add divisor
			add_in_place(mut remainder, operand_b)
			unsafe {goto end}
		}
	} else { // cmp > 0: the remainder is positive and < quotient => finished
		subtract_digit_array(operand_a, product, mut remainder)
		if compare_digit_array(remainder, operand_b) < 0 {
			unsafe {goto end}
		}
	}
	println('quot: $quotient rem: $remainder derivate: $derivate')

	// ajust result with the differential
	// estimate slope the first time
	if step == 0 {
		clear_first_bits_and_set_some(remainder, operand_b_bit_len, mut derivate)
		for i in 0 .. remainder.len {
			save_difference[i] = remainder[i]
		}
		for save_difference.len > remainder.len {
			save_difference.delete_last()
		}
		step++
	} else {
		// // divide derivata by 2
		// rshift_in_place(mut derivate, 1)
		// calculate exact slope
		subtract_in_place(mut save_difference, remainder)
		println('diff: $save_difference derivate: $derivate')
		for remainder.len > 0 {
			remainder.delete_last() // prepare divide_digit_array
		}
		for slope.len > 0 {
			slope.delete_last()
		}
		divide_digit_array(derivate, save_difference, mut slope, mut remainder) // ATTENTION reentrance but much smaller numbers
		for i in 0 .. slope.len {
			derivate[i] = slope[i]
		}
		for derivate.len > slope.len {
			derivate.delete_last()
		}
	}
	if cmp > 0 {
		add_in_place(mut quotient, derivate)
	} else {
		subtract_in_place(mut quotient, derivate)
	}
	unsafe {goto loop}

end:
	for remainder.len > 0 && remainder.last() == 0 {
		remainder.delete_last()
	}
	for quotient.len > 0 && quotient.last() == 0 {
		quotient.delete_last()
	}
	unsafe {goto ret}

null_remainder:
	for remainder.len > 0 {
		remainder.delete_last()
	}
ret:
}

// operand b can be greater than operand a
// the capacity of both array is sufficient
[inline]
fn add_in_place(mut a []u32, b []u32) {
	len_a := a.len
	len_b := b.len
	max := util.imax(len_a, len_b)
	min := util.imin(len_a, len_b)
	mut carry := u64(0)
	for index in 0 .. min {
		partial := carry + a[index] + b[index]
		a[index] = u32(partial)
		carry = u32(partial >> 32)
	}
	if len_a >= len_b {
		for index in min .. max {
			partial := carry + a[index]
			a[index] = u32(partial)
			carry = u32(partial >> 32)
		}
	} else {
		for index in min .. max {
			partial := carry + b[index]
			a << u32(partial)
			carry = u32(partial >> 32)
		}
	}
}

// a := a - b if a < b return the abs of the result
[inline]
fn subtract_in_place(mut a []u32, b []u32) {
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
	if carry != 0 {
		neg_in_place(mut a)
	}
}

[inline]
fn neg_in_place(mut a []u32) {
	mut carry := u32(0)
	for i in 0 .. a.len {
		partial := a[i] + carry
		carry = if int(partial) > 0 {u32(1)} else {u32(0)}
		a[i] = - partial
	}
}

// dest must be already initialized with sufficient length
[inline]
fn 	clear_first_bits_and_set_some(src []u32, length int, mut dest []u32) {
	src_bit_len := src.len * 32 - bits.leading_zeros_32(src.last())
	dest_bit_len := src_bit_len - length
	byte_len := int(dest_bit_len / 32)
	// println('src_bit_len: $src_bit_len des_bit_len: $dest_bit_len byte_len: $byte_len')
	for i in 0 .. byte_len {
		dest[i] = src[i]
	}
	for i in byte_len .. dest.len {
		dest[i] = u32(0)
	}
	// println('after copying: $dest')
	last := src[byte_len]
	mask := u32((u64(1) << (dest_bit_len % 32)) - 1)
	dest[byte_len] = u32(last & mask)
	// println('after making: $dest')
	for i in 1 .. 4 {		// set the first 3 significand bit to not fail the scope
		bit_set(mut dest, dest_bit_len - i)
	}
	// println('after setting bits: $dest')
}