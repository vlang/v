module big

import math.bits

// Compares the magnitude of the two unsigned integers represented the given
// digit arrays. Returns -1 if a < b, 0 if a == b and +1 if a > b. Here
// a is operand_a and b is operand_b (for brevity).
@[direct_array_access]
fn compare_digit_array(operand_a []u64, operand_b []u64) int {
	a_len := operand_a.len
	b_len := operand_b.len
	if a_len != b_len {
		return if a_len < b_len { -1 } else { 1 }
	}
	// They have the same number of digits now
	// Go from the most significant digit to the least significant one
	for index := a_len - 1; index >= 0; index-- {
		a_digit := operand_a[index]
		b_digit := operand_b[index]
		if a_digit != b_digit {
			return if a_digit < b_digit { -1 } else { 1 }
		}
	}
	return 0
}

// Add the digits in operand_a and operand_b and stores the result in sum.
// This function does not perform any allocation and assumes that the storage is
// large enough. It may affect the last element, based on the presence of a carry
@[direct_array_access]
fn add_digit_array(operand_a []u64, operand_b []u64, mut sum []u64) {
	// Zero length cases
	if operand_a.len == 0 {
		for index in 0 .. operand_b.len {
			sum[index] = operand_b[index]
		}
		shrink_tail_zeros(mut sum)
		return
	}
	if operand_b.len == 0 {
		for index in 0 .. operand_a.len {
			sum[index] = operand_a[index]
		}
		shrink_tail_zeros(mut sum)
		return
	}

	mut a, mut b := if operand_a.len >= operand_b.len {
		operand_a, operand_b
	} else {
		operand_b, operand_a
	}
	mut carry := u64(0)
	for index in 0 .. b.len {
		partial := carry + a[index] + b[index]
		sum[index] = partial & max_digit
		carry = partial >> digit_bits
	}

	for index in b.len .. a.len {
		partial := carry + a[index]
		sum[index] = partial & max_digit
		carry = partial >> digit_bits
	}

	sum[a.len] = carry
	shrink_tail_zeros(mut sum)
}

// Subtracts operand_b from operand_a and stores the difference in storage.
// It assumes operand_a contains the larger "integer" and that storage is
// the same size as operand_a and is 0
@[direct_array_access]
fn subtract_digit_array(operand_a []u64, operand_b []u64, mut storage []u64) {
	// Zero length cases
	if operand_a.len == 0 {
		// nothing to subtract from
		return
	}
	if operand_b.len == 0 {
		// nothing to subtract
		for index in 0 .. operand_a.len {
			storage[index] = operand_a[index]
		}
		return
	}

	mut borrow := u64(0)
	for index in 0 .. operand_b.len {
		a := operand_a[index]
		b := operand_b[index] + borrow
		diff := a - b
		borrow = (diff >> digit_bits) & 1
		storage[index] = diff + (borrow << digit_bits)
	}
	for index in operand_b.len .. operand_a.len {
		diff := operand_a[index] - borrow
		borrow = (diff >> digit_bits) & 1
		storage[index] = diff + (borrow << digit_bits)
	}

	shrink_tail_zeros(mut storage)
}

const karatsuba_multiplication_limit = 70

const toom3_multiplication_limit = 360

@[inline]
fn multiply_digit_array(operand_a []u64, operand_b []u64, mut storage []u64) {
	max_len := if operand_a.len >= operand_b.len {
		operand_a.len
	} else {
		operand_b.len
	}
	if max_len >= toom3_multiplication_limit {
		toom3_multiply_digit_array(operand_a, operand_b, mut storage)
	} else if max_len >= karatsuba_multiplication_limit {
		karatsuba_multiply_digit_array(operand_a, operand_b, mut storage)
	} else {
		simple_multiply_digit_array(operand_a, operand_b, mut storage)
	}
}

// Multiplies the unsigned (non-negative) integers represented in a and b and the product is
// stored in storage. It assumes that storage has length equal to the sum of lengths
// of a and b. Length refers to length of array, that is, digit count.
@[direct_array_access]
fn simple_multiply_digit_array(operand_a []u64, operand_b []u64, mut storage []u64) {
	for b_index in 0 .. operand_b.len {
		mut hi := u64(0)
		mut lo := u64(0)
		for a_index in 0 .. operand_a.len {
			hi, lo = bits.mul_add_64(operand_a[a_index], operand_b[b_index], storage[a_index +
				b_index] + hi)
			storage[a_index + b_index] = lo & max_digit
			hi = (hi << (64 - digit_bits)) | (lo >> digit_bits)
		}
		if hi != 0 {
			storage[b_index + operand_a.len] = hi
		}
	}
	shrink_tail_zeros(mut storage)
}

// Stores the product of the unsigned (non-negative) integer represented in a and the digit in value
// in the storage array. It assumes storage is pre-initialised and populated with 0's
@[direct_array_access]
fn multiply_array_by_digit(operand_a []u64, value u64, mut storage []u64) {
	if value == 0 {
		storage.clear()
		return
	}
	if value == 1 {
		for index in 0 .. operand_a.len {
			storage[index] = operand_a[index]
		}
		shrink_tail_zeros(mut storage)
		return
	}
	mut hi := u64(0)
	mut lo := u64(0)
	for index in 0 .. operand_a.len {
		hi, lo = bits.mul_add_64(operand_a[index], value, hi)
		storage[index] = lo & max_digit
		hi = hi << (64 - digit_bits) + (lo >> digit_bits)
	}

	if hi > 0 {
		storage[operand_a.len] = hi
	}
	shrink_tail_zeros(mut storage)
}

// Divides the non-negative integer in a by non-negative integer b and store the two results
// in quotient and remainder respectively. It is different from the rest of the functions
// because it assumes that quotient and remainder are empty zero length arrays. They can be
// made to have appropriate capacity though
@[direct_array_access]
fn divide_digit_array(operand_a []u64, operand_b []u64, mut quotient []u64, mut remainder []u64) {
	cmp_result := compare_digit_array(operand_a, operand_b)
	// a == b => q, r = 1, 0
	if cmp_result == 0 {
		quotient << 1
		for quotient.len > 1 {
			quotient.delete_last()
		}
		remainder.clear()
		return
	}

	// a < b => q, r = 0, a
	if cmp_result < 0 {
		quotient.clear()
		remainder << operand_a
		return
	}
	if operand_b.len == 1 {
		divide_array_by_digit(operand_a, operand_b[0], mut quotient, mut remainder)
	} else {
		divide_array_by_array(operand_a, operand_b, mut quotient, mut remainder)
	}
}

// Performs division on the non-negative dividend in a by the single digit divisor b. It assumes
// quotient and remainder are empty zero length arrays without previous allocation
@[direct_array_access]
fn divide_array_by_digit(operand_a []u64, divisor u64, mut quotient []u64, mut remainder []u64) {
	if operand_a.len == 1 {
		// 1 digit for both dividend and divisor
		dividend := operand_a[0]
		q := dividend / divisor
		if q != 0 {
			quotient << q
		}
		rem := dividend % divisor
		if rem != 0 {
			remainder << rem
		}
		return
	}
	// Dividend has more digits
	mut rem := u64(0)
	mut quo := u64(0)
	mut qtemp := []u64{len: quotient.cap}
	divisor64 := u64(divisor)

	// Perform division step by step
	for index := operand_a.len - 1; index >= 0; index-- {
		hi := rem >> (64 - digit_bits)
		lo := rem << digit_bits | operand_a[index]
		quo, rem = bits.div_64(hi, lo, divisor64)
		qtemp[index] = quo & max_digit
	}
	// Remove leading zeros from quotient
	shrink_tail_zeros(mut qtemp)
	quotient << qtemp
	remainder << rem
	shrink_tail_zeros(mut remainder)
}

@[inline]
fn divide_array_by_array(operand_a []u64, operand_b []u64, mut quotient []u64, mut remainder []u64) {
	binary_divide_array_by_array(operand_a, operand_b, mut quotient, mut remainder)
}

// Shifts the contents of the original array by the given amount of bits to the left.
// This function assumes that the amount is less than `digit_bits`. The storage is expected to
// allocated with zeroes.
@[direct_array_access]
fn shift_digits_left(original []u64, amount u32, mut storage []u64) {
	mut leftover := u64(0)
	offset := digit_bits - amount
	for index in 0 .. original.len {
		value := (leftover | (original[index] << amount)) & max_digit
		leftover = (original[index] & (u64(-1) << offset)) >> offset
		storage[index] = value
	}
	if leftover != 0 {
		storage << leftover
	}
}

// Shifts the contents of the original array by the given amount of bits to the right.
// This function assumes that the amount is less than `digit_bits`. The storage is expected to
// be allocated with zeroes.
@[direct_array_access]
fn shift_digits_right(original []u64, amount u32, mut storage []u64) {
	mut moveover := u64(0)
	mask := (u64(1) << amount) - 1
	offset := digit_bits - amount
	for index := original.len - 1; index >= 0; index-- {
		value := (moveover << offset) | (original[index] >> amount)
		moveover = original[index] & mask
		storage[index] = value
	}
	shrink_tail_zeros(mut storage)
}

@[direct_array_access]
fn bitwise_or_digit_array(operand_a []u64, operand_b []u64, mut storage []u64) {
	lower, upper, bigger := if operand_a.len < operand_b.len {
		operand_a.len, operand_b.len, operand_b
	} else {
		operand_b.len, operand_a.len, operand_a
	}
	for index in 0 .. lower {
		storage[index] = operand_a[index] | operand_b[index]
	}
	for index in lower .. upper {
		storage[index] = bigger[index]
	}
	shrink_tail_zeros(mut storage)
}

@[direct_array_access]
fn bitwise_and_digit_array(operand_a []u64, operand_b []u64, mut storage []u64) {
	lower := imin(operand_a.len, operand_b.len)
	for index in 0 .. lower {
		storage[index] = operand_a[index] & operand_b[index]
	}
	shrink_tail_zeros(mut storage)
}

@[direct_array_access]
fn bitwise_xor_digit_array(operand_a []u64, operand_b []u64, mut storage []u64) {
	lower, upper, bigger := if operand_a.len < operand_b.len {
		operand_a.len, operand_b.len, operand_b
	} else {
		operand_b.len, operand_a.len, operand_a
	}
	for index in 0 .. lower {
		storage[index] = operand_a[index] ^ operand_b[index]
	}
	for index in lower .. upper {
		storage[index] = bigger[index]
	}
	shrink_tail_zeros(mut storage)
}

@[direct_array_access]
fn bitwise_not_digit_array(original []u64, mut storage []u64) {
	for index in 0 .. original.len {
		storage[index] = (~original[index]) & max_digit
	}
	shrink_tail_zeros(mut storage)
}
