module big

import math.bits
import math.util
import strings


const t_1 = f64(48)/17
const t_2 = f64(32)/17

// suppose operand_a bigger than operand_b and both not null.
// Both quotient and remaider are allocated but of length 0
fn divide_array_by_array(operand_a []u32, operand_b []u32, mut quotient []u32, mut remainder []u32) {

// Pseudo code
// Express D as M × 2e where 1 ≤ M < 2 (standard floating point representation)
// D' := D / 2e+1   // scale between 0.5 and 1, can be performed with bit shift / exponent subtraction
// N' := N / 2e+1
// X := 48/17 − 32/17 × D'   // precompute constants with same precision as D
// repeat ⌈ log 2 ⁡ P + 1 log 2 ⁡ 17 ⌉ {\displaystyle \left\lceil \log _{2}{\frac {P+1}{\log _{2}17}}\right\rceil \,} \left\lceil \log _{2}{\frac {P+1}{\log _{2}17}}\right\rceil \, times   // can be precomputed based on fixed P
//     X := X + X × (1 - D' × X)
// end
// return N' × X

// python
// def unsigned_div_newton(a, b):
//     assert a >= 0 and b > 0
//     if a <= b:
//         return int(a == b)

//     k = a.bit_length() + b.bit_length()  # a*b < 2**k
//     x = 2  #  0 < x < 2**(k+1)/b  # initial guess for convergence
//     lastx = None
//     while lastx != x:
//         lastx = x
//         x = (x * (2**(k + 1) - x * b)) >> k
//     if x*b < 2**k: 
//         x += 1
//     return (a * x) >> k

// 	V code working with u64 integers
// fn unsigned_div_newton(a u64, b u64) u64 {
//     assert a >= 0 && b > 0
//     if a <= b {
//         return 0
// 	}
//     k := bit_length(a) + bit_length(b)  // a*b < 2**k
//     mut x := u64(2)  //  0 < x < 2**(k+1)/b  // initial guess for convergence
//     mut lastx := u64(0)
//     for lastx != x {
//         lastx = x
//         x = (x * (pow2(k + 1) - x * b)) >> k
// 	}
//     if x*b < pow2(k) {
//         x += 1
// 	}
//     return (a * x) >> k
// }

	// tranform back to Integers (local without allocation)
	a := Integer{signum: 1, digits: operand_a}
	b := Integer{signum: 1, digits: operand_b}
	// we cannot use quotient or remainder; they create bug because they are not null
	// mut q := Integer{signum: 1, digits: quotient}
	// mut r := Integer{signum: 1, digits: remainder}

    k := bit_length(a) + bit_length(b)  // a*b < 2**k
    mut x := integer_from_int(2)  //  0 < x < 2**(k+1)/b  // initial guess for convergence
	initial_guess :=  (((integer_from_int(48) - (integer_from_int(32) * b)) * integer_from_i64(0x0f0f0f0f0f0f0f0f)).rshift(64)).neg() // / 17 == 0x11
	if initial_guess > zero_int {
		x = initial_guess
	}
	mut lastx := integer_from_int(0)
	mut counter := 0
	// println('------------------------------')
	// println('k: ${k}')
	// println('initial guess:${initial_guess.signum} ${debug_u32_str(initial_guess.digits)}')
	// println('x$counter: ${x.signum} ${debug_u32_str(x.digits)}')
    for lastx != x {
        lastx = x
        x = (x * (pow2(k + 1) - x * b)).rshift(u32(k))
		counter++
		// println('x$counter: ${debug_u32_str(x.digits)}')
	}
    if x*b < pow2(k) {
        x.inc()
	}
    mut q := (a * x).rshift(u32(k))
	// println('q * b: ${debug_u32_str((q * b).digits)}')
	if q * b > a {
		q.dec()
		// println('q.dec(): ${debug_u32_str(q.digits)}')
	}
	// println('a:${a.signum}, ${debug_u32_str(a.digits)}')
	// println('(q * b): ${debug_u32_str((q * b).digits)}')
	mut r := a - (q * b)
	// println('r:${r.signum}, ${debug_u32_str(r.digits)}')
	// println('q: ${debug_u32_str(q.digits)}')
	if r >= b {
		q.inc()
		r -= b
		// println('q.inc(): ${debug_u32_str(q.digits)}')
		// println('\tb: ${debug_u32_str(b.digits)}')
		// println('\tr:${r.signum}, ${debug_u32_str(r.digits)}')
	}

	// control
	// println('initial guess:${initial_guess.signum} ${debug_u32_str(initial_guess.digits)}')
	// println('bit_length initial guess: ${bit_length(initial_guess)}')
	// println('bit_length x: ${bit_length(x)}')
	// for returning []u32
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
		if ! first { sb.write_string(', ') }
		sb.write_string('0x${a[i].hex()}')
		first = false
	}
	sb.write_string(']')
	return sb.str()
}

fn multiply_kara_simpl(a Integer, b Integer) Integer {
	// base case
	if a.digits.len == 0 || b.digits.len == 0 {
		return zero_int
	}

	if a == one_int {
		return b
	}

	if b == one_int {
		return a
	}

	if b.signum < 0 {
		return multiply_kara_simpl(a, b.neg()).neg()
	}

	if b.signum < 0 {
		return multiply_kara_simpl(a.neg(), b).neg()
	}

	if a < b {
		return multiply_kara_simpl(b, a)
	}

	if b.digits.len <= 1 {
		mut c := []u32{len: a.digits.len + b.digits.len + 1, init: 0}
		multiply_array_by_digit(a.digits, b.digits[0], mut c)
		return Integer{signum: 1, digits: c}
	}
	// karatsuba
	// through the base cases we can pass zero-length arrays to the mult func
	half := util.imax(a.digits.len, b.digits.len) / 2
	if half <= 0 {
		panic('Unreachable. Both array have 1 length and multiply_array_by_digit should have been called')
	} else {
		// println('------------------------------')
// 		println('a: ${debug_u32_str(a.digits)}')
		a_l := Integer{signum: 1, digits: a.digits[0..half]}
		// println('a_l: ${debug_u32_str(a_l.digits)}')
		a_h := Integer{signum: 1, digits: a.digits[half..]}
		// println('a_h: ${debug_u32_str(a_h.digits)}')
		// println('b: ${debug_u32_str(b.digits)}')
		b_l := Integer{signum: 1, digits: b.digits[0..half]}
		// println('b_l: ${debug_u32_str(b_l.digits)}')
		b_h := Integer{signum: 1, digits: b.digits[half..]}
		// println('b_h: ${debug_u32_str(b_h.digits)}')

		p_1 := multiply_kara_simpl(a_h, b_h)
		// println('p_1: ${debug_u32_str(p_1.digits)}')
		p_3 := multiply_kara_simpl(a_l, b_l)
		// println('p_3: ${debug_u32_str(p_3.digits)}')
		p_2 := multiply_kara_simpl(a_h + a_l, b_h + b_l) - p_1 - p_3
		// println('p_3: ${debug_u32_str(p_3.digits)}')

		return p_1.lshift(2 * u32(half * 32)) + p_2.lshift(u32(half * 32)) + p_3
	}
}

// operations with already allocated result
// fn subtract_2(a  Integer, b Integer, mut c Integer, length int) {
// 	clear_u32_array(mut c.digits, length)
// 	if a.signum == b.signum {
// 		if a.abs_cmp(b) >= 0 {
// 			subtract_digit_array(a.digits, b.digits, mut c.digits)
// 			c.signum = a.signum
// 		} else {
// 			subtract_digit_array(b.digits, a.digits, mut c.digits)
// 			c.signum = - a.signum
// 		}
// 	} else {
// 		add_digit_array(a.digits, b.digits, mut c.digits)
// 		c.signum = a.signum
// 	}
// }

// fn multiply_2(a Integer, b Integer, mut c Integer, length int) {
// 	clear_u32_array(mut c.digits, length)
// 	multiply_digit_array(a.digits, b.digits, mut c.digits)
// 	c.signum = if a.signum == b.signum { 1 } else { -1 }
// }

fn copy_array(mut dest []u32, src []u32) {
	min_len := util.imin(dest.len, src.len)
	for i in 0 .. min_len {
		dest[i] = src[i]
	}
	for i in min_len .. src.len { // could be empty
		dest << src[i]
	}
}

fn clear_u32_array(mut ar []u32, length int) {
	for i in 0 .. ar.len {
		ar[i] = u32(0)
	}
	for _ in ar.len .. length {
		ar << u32(0)
	}
}

fn pow2(k int) Integer {
	mut ret := []u32{len: k / 32 + 1, init: 0}
	bit_set(mut ret, k)
	return Integer{signum: 1, digits: ret}
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