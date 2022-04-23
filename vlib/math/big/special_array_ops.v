module big

import math
import math.bits
import strings

// suppose operand_a bigger than operand_b and both not null.
// Both quotient and remaider are already allocated but of length 0
fn newton_divide_array_by_array(operand_a []u32, operand_b []u32, mut quotient []u32, mut remainder []u32) {
	// tranform back to Integers (on the stack without allocation)
	a := Integer{
		signum: 1
		digits: operand_a
	}
	b := Integer{
		signum: 1
		digits: operand_b
	}

	k := bit_length(a) + bit_length(b) // a*b < 2**k
	mut x := integer_from_int(2) //  0 < x < 2**(k+1)/b  // initial guess for convergence
	// https://en.wikipedia.org/wiki/Division_algorithm#Newton%E2%80%93Raphson_division
	// use 48/17 - 32/17.D (divisor)
	initial_guess := (((integer_from_int(48) - (integer_from_int(32) * b)) * integer_from_i64(0x0f0f0f0f0f0f0f0f)).rshift(64)).neg() // / 17 == 0x11
	if initial_guess > zero_int {
		x = initial_guess
	}
	mut lastx := integer_from_int(0)
	pow2_k_plus_1 := pow2(k + 1) // outside of the loop to optimize allocatio
	for lastx != x { // main loop
		lastx = x
		x = (x * (pow2_k_plus_1 - (x * b))).rshift(u32(k))
	}
	if x * b < pow2(k) {
		x.inc()
	}
	mut q := (a * x).rshift(u32(k))
	// possible adjustments. see literature
	if q * b > a {
		q.dec()
	}
	mut r := a - (q * b)
	if r >= b {
		q.inc()
		r -= b
	}
	quotient = q.digits
	remainder = r.digits

	for remainder.len > 0 && remainder.last() == 0 {
		remainder.delete_last()
	}
}

[inline]
fn bit_length(a Integer) int {
	return a.digits.len * 32 - bits.leading_zeros_32(a.digits.last())
}

[inline]
fn debug_u32_str(a []u32) string {
	mut sb := strings.new_builder(30)
	sb.write_string('[')
	mut first := true
	for i in 0 .. a.len {
		if !first {
			sb.write_string(', ')
		}
		sb.write_string('0x${a[i].hex()}')
		first = false
	}
	sb.write_string(']')
	return sb.str()
}

// karatsuba algorithm for multiplication
// possible optimisations:
// - transform one or all the recurrences in loops
fn karatsuba_multiply_digit_array(operand_a []u32, operand_b []u32, mut storage []u32) {
	// base case necessary to end recursion
	if operand_a.len == 0 || operand_b.len == 0 {
		for storage.len > 0 {
			storage.delete_last()
		}
		return
	}

	if operand_a.len < operand_b.len {
		multiply_digit_array(operand_b, operand_a, mut storage)
		return
	}

	if operand_b.len == 1 {
		multiply_array_by_digit(operand_a, operand_b[0], mut storage)
		return
	}
	// karatsuba
	// thanks to the base cases we can pass zero-length arrays to the mult func
	half := math.max(operand_a.len, operand_b.len) / 2
	if half <= 0 {
		panic('Unreachable. Both array have 1 length and multiply_array_by_digit should have been called')
	}
	a_l := operand_a[0..half]
	a_h := operand_a[half..]
	mut b_l := []u32{}
	mut b_h := []u32{}
	if half <= operand_b.len {
		b_l = operand_b[0..half]
		b_h = operand_b[half..]
	} else {
		b_l = unsafe { operand_b }
		// b_h = []u32{}
	}

	// use storage for p_1 to avoid allocation and copy later
	multiply_digit_array(a_h, b_h, mut storage)

	mut p_3 := []u32{len: a_l.len + b_l.len + 1, init: 0}
	multiply_digit_array(a_l, b_l, mut p_3)

	mut tmp_1 := []u32{len: math.max(a_h.len, a_l.len) + 1, init: 0}
	mut tmp_2 := []u32{len: math.max(b_h.len, b_l.len) + 1, init: 0}
	add_digit_array(a_h, a_l, mut tmp_1)
	add_digit_array(b_h, b_l, mut tmp_2)

	mut p_2 := []u32{len: operand_a.len + operand_b.len + 1, init: 0}
	multiply_digit_array(tmp_1, tmp_2, mut p_2)
	subtract_in_place(mut p_2, storage) // p_1
	subtract_in_place(mut p_2, p_3)

	// return p_1.lshift(2 * u32(half * 32)) + p_2.lshift(u32(half * 32)) + p_3
	lshift_byte_in_place(mut storage, 2 * half)
	lshift_byte_in_place(mut p_2, half)
	add_in_place(mut storage, p_2)
	add_in_place(mut storage, p_3)

	for storage.len > 0 && storage.last() == 0 {
		storage.delete_last()
	}
}

[inline]
fn pow2(k int) Integer {
	mut ret := []u32{len: (k >> 5) + 1, init: 0}
	bit_set(mut ret, k)
	return Integer{
		signum: 1
		digits: ret
	}
}

// optimized left shift of full u8(s) in place. byte_nb must be positive
fn lshift_byte_in_place(mut a []u32, byte_nb int) {
	a_len := a.len
	// control or allocate capacity
	for _ in a_len .. a_len + byte_nb {
		a << u32(0)
	}
	for index := a_len - 1; index >= 0; index-- {
		a[index + byte_nb] = a[index]
	}
	for index in 0 .. byte_nb {
		a[index] = u32(0)
	}
}

// operand b can be greater than operand a
// the capacity of both array is supposed to be sufficient
[inline]
fn add_in_place(mut a []u32, b []u32) {
	len_a := a.len
	len_b := b.len
	max := math.max(len_a, len_b)
	min := math.min(len_a, len_b)
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

// a := a - b supposed a >= b
fn subtract_in_place(mut a []u32, b []u32) {
	len_a := a.len
	len_b := b.len
	max := math.max(len_a, len_b)
	min := math.min(len_a, len_b)
	mut carry := u32(0)
	mut new_carry := u32(0)
	for index in 0 .. min {
		if a[index] < (b[index] + carry) {
			new_carry = 1
		} else {
			new_carry = 0
		}
		a[index] -= (b[index] + carry)
		carry = new_carry
	}
	if len_a >= len_b {
		for index in min .. max {
			if a[index] < carry {
				new_carry = 1
			} else {
				new_carry = 0
			}
			a[index] -= carry
			carry = new_carry
		}
	} else { // if len.b > len.a return zero
		for a.len > 0 {
			a.delete_last()
		}
	}
}
