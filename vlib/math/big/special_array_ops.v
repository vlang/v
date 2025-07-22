module big

import strings

@[direct_array_access; inline]
fn shrink_tail_zeros(mut a []u32) {
	mut alen := a.len
	for alen > 0 && a[alen - 1] == 0 {
		alen--
	}
	unsafe {
		a.len = alen
	}
}

@[direct_array_access; inline]
fn (i &Integer) shrink_tail_zeros() {
	mut alen := i.digits.len
	for alen > 0 && i.digits[alen - 1] == 0 {
		alen--
	}
	unsafe {
		i.digits.len = alen
	}
}

// suppose operand_a bigger than operand_b and both not null.
// Both quotient and remaider are already allocated but of length 0
// TODO: the manualfree tag here is a workaround for compilation with -autofree. Remove it, when the -autofree bug is fixed.
@[manualfree]
fn newton_divide_array_by_array(operand_a []u32, operand_b []u32, mut quotient []u32, mut remainder []u32) {
	// transform back to Integers (on the stack without allocation)
	a := Integer{
		signum: 1
		digits: operand_a
	}
	b := Integer{
		signum: 1
		digits: operand_b
	}

	k := a.bit_len() + b.bit_len() // a*b < 2**k
	mut x := integer_from_int(2) //  0 < x < 2**(k+1)/b  // initial guess for convergence
	// https://en.wikipedia.org/wiki/Division_algorithm#Newton%E2%80%93Raphson_division
	// use 48/17 - 32/17.D (divisor)
	initial_guess := (((integer_from_int(48) - (integer_from_int(32) * b)) * integer_from_i64(0x0f0f0f0f0f0f0f0f)).right_shift(64)).neg() // / 17 == 0x11
	if initial_guess > zero_int {
		x = initial_guess
	}
	mut lastx := integer_from_int(0)
	pow2_k_plus_1 := pow2(k + 1) // outside of the loop to optimize allocatio
	for lastx != x { // main loop
		lastx = x
		x = (x * (pow2_k_plus_1 - (x * b))).right_shift(u32(k))
	}
	if x * b < pow2(k) {
		x.inc()
	}
	mut q := (a * x).right_shift(u32(k))
	// possible adjustments. see literature
	if q * b > a {
		q.dec()
	}
	mut r := a - (q * b)
	if r >= b {
		q.inc()
		r -= b
	}
	quotient = q.digits.clone()
	remainder = r.digits.clone()

	shrink_tail_zeros(mut remainder)
}

@[direct_array_access; inline]
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

@[direct_array_access; inline]
fn found_multiplication_base_case(operand_a []u32, operand_b []u32, mut storage []u32) bool {
	// base case necessary to end recursion
	if operand_a.len == 0 || operand_b.len == 0 {
		storage.clear()
		return true
	}

	if operand_a.len < operand_b.len {
		multiply_digit_array(operand_b, operand_a, mut storage)
		return true
	}

	if operand_b.len == 1 {
		multiply_array_by_digit(operand_a, operand_b[0], mut storage)
		return true
	}
	return false
}

// karatsuba algorithm for multiplication
// possible optimisations:
// - transform one or all the recurrences in loops
@[direct_array_access]
fn karatsuba_multiply_digit_array(operand_a []u32, operand_b []u32, mut storage []u32) {
	if found_multiplication_base_case(operand_a, operand_b, mut storage) {
		return
	}

	// thanks to the base cases we can pass zero-length arrays to the mult func
	half := imax(operand_a.len, operand_b.len) / 2
	mut a_l := unsafe { operand_a[0..half] }
	mut a_h := unsafe { operand_a[half..] }
	mut b_l := []u32{}
	mut b_h := []u32{}
	if half <= operand_b.len {
		b_l = unsafe { operand_b[0..half] }
		b_h = unsafe { operand_b[half..] }
	} else {
		b_l = unsafe { operand_b }
		// b_h = []u32{}
	}
	shrink_tail_zeros(mut a_l)
	shrink_tail_zeros(mut a_h)
	shrink_tail_zeros(mut b_l)
	shrink_tail_zeros(mut b_h)

	// use storage for p_1 to avoid allocation and copy later
	multiply_digit_array(a_h, b_h, mut storage)

	mut p_3 := []u32{len: a_l.len + b_l.len + 1}
	multiply_digit_array(a_l, b_l, mut p_3)

	mut tmp_1 := []u32{len: imax(a_h.len, a_l.len) + 1}
	mut tmp_2 := []u32{len: imax(b_h.len, b_l.len) + 1}
	add_digit_array(a_h, a_l, mut tmp_1)
	add_digit_array(b_h, b_l, mut tmp_2)

	mut p_2 := []u32{len: operand_a.len + operand_b.len + 1}
	multiply_digit_array(tmp_1, tmp_2, mut p_2)
	subtract_in_place(mut p_2, storage) // p_1
	subtract_in_place(mut p_2, p_3)

	// return p_1.left_shift(2 * u32(half * 32)) + p_2.left_shift(u32(half * 32)) + p_3
	left_shift_digits_in_place(mut storage, 2 * half)
	left_shift_digits_in_place(mut p_2, half)
	add_in_place(mut storage, p_2)
	add_in_place(mut storage, p_3)

	shrink_tail_zeros(mut storage)
}

// TODO: the manualfree tag here is a workaround for compilation with -autofree. Remove it, when the -autofree bug is fixed.
@[direct_array_access; manualfree]
fn toom3_multiply_digit_array(operand_a []u32, operand_b []u32, mut storage []u32) {
	if found_multiplication_base_case(operand_a, operand_b, mut storage) {
		return
	}

	// After the base case, we have operand_a as the larger integer in terms of digit length

	// k is the length (in u32 digits) of the lower order slices
	k := (operand_a.len + 2) / 3
	k2 := 2 * k

	// The pieces of the calculation need to be worked on as proper big.Integers
	// because the intermediate results can be negative. After recombination, the
	// final result will be positive.

	// Slices of a and b
	a0 := Integer{
		digits: unsafe { operand_a[..k] }
		signum: if operand_a[..k].all(it == 0) {
			0
		} else {
			1
		}
	}
	a0.shrink_tail_zeros()
	a1 := Integer{
		digits: unsafe { operand_a[k..k2] }
		signum: if operand_a[k..k2].all(it == 0) {
			0
		} else {
			1
		}
	}
	a1.shrink_tail_zeros()
	a2 := Integer{
		digits: unsafe { operand_a[k2..] }
		signum: 1
	}

	// Zero arrays by default
	mut b0 := zero_int.clone()
	mut b1 := zero_int.clone()
	mut b2 := zero_int.clone()

	if operand_b.len < k {
		b0 = Integer{
			digits: operand_b
			signum: 1
		}
	} else if operand_b.len < k2 {
		if !operand_b[..k].all(it == 0) {
			b0 = Integer{
				digits: operand_b[..k]
				signum: 1
			}
		}
		b0.shrink_tail_zeros()
		b1 = Integer{
			digits: operand_b[k..]
			signum: 1
		}
	} else {
		if !operand_b[..k].all(it == 0) {
			b0 = Integer{
				digits: operand_b[..k]
				signum: 1
			}
		}
		b0.shrink_tail_zeros()
		if !operand_b[k..k2].all(it == 0) {
			b1 = Integer{
				digits: operand_b[k..k2]
				signum: 1
			}
		}
		b1.shrink_tail_zeros()
		b2 = Integer{
			digits: operand_b[k2..]
			signum: 1
		}
	}

	// https://en.wikipedia.org/wiki/Toom%E2%80%93Cook_multiplication#Details
	// DOI: 10.1007/978-3-540-73074-3_10

	p0 := a0 * b0
	mut ptemp := a2 + a0
	mut qtemp := b2 + b0
	vm1 := (ptemp - a1) * (qtemp - b1)
	ptemp += a1
	qtemp += b1
	p1 := ptemp * qtemp
	p2 := ((ptemp + a2).left_shift(1) - a0) * ((qtemp + b2).left_shift(1) - b0)
	pinf := a2 * b2

	mut t2, _ := (p2 - vm1).div_mod_internal(three_int)
	mut tm1 := (p1 - vm1).right_shift(1)
	mut t1 := p1 - p0
	t2 = (t2 - t1).right_shift(1)
	t1 = (t1 - tm1 - pinf)
	t2 = t2 - pinf.left_shift(1)
	tm1 = tm1 - t2

	// shift amount
	s := u32(k) << 5

	result := (((pinf.left_shift(s) + t2).left_shift(s) + t1).left_shift(s) + tm1).left_shift(s) +
		p0

	storage = result.digits.clone()
}

@[inline]
fn pow2(k int) Integer {
	mut ret := []u32{len: (k >> 5) + 1}
	bit_set(mut ret, k)
	return Integer{
		signum: 1
		digits: ret
	}
}

// optimized left shift in place. amount must be positive
fn left_shift_digits_in_place(mut a []u32, amount int) {
	// this is actual in builtin/array.v, prepend_many (private fn)
	// x := []u32{ len : amount }
	// a.prepend_many(&x[0], amount)
	old_len := a.len
	elem_size := a.element_size
	unsafe {
		a.grow_len(amount)
		sptr := &u8(a.data)
		dptr := &u8(a.data) + u64(amount) * u64(elem_size)
		vmemmove(dptr, sptr, u64(old_len) * u64(elem_size))
		vmemset(sptr, 0, u64(amount) * u64(elem_size))
	}
}

// optimized right shift in place. amount must be positive
fn right_shift_digits_in_place(mut a []u32, amount int) {
	a.drop(amount)
}

// operand b can be greater than operand a
// the capacity of both array is supposed to be sufficient
@[direct_array_access; inline]
fn add_in_place(mut a []u32, b []u32) {
	len_a := a.len
	len_b := b.len
	max := imax(len_a, len_b)
	min := imin(len_a, len_b)
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
	if carry > 0 {
		a << u32(carry)
	}
}

// a := a - b supposed a >= b
@[direct_array_access; inline]
fn subtract_in_place(mut a []u32, b []u32) {
	len_a := a.len
	len_b := b.len
	max := imax(len_a, len_b)
	min := imin(len_a, len_b)

	mut borrow := false
	for index in 0 .. min {
		mut a_digit := u64(a[index])
		b_digit := b[index] + if borrow { u64(1) } else { u64(0) }
		borrow = a_digit < b_digit
		if borrow {
			a_digit += 0x100000000
		}
		a[index] = u32(a_digit - b_digit)
	}

	if len_a >= len_b {
		for index in min .. max {
			mut a_digit := u64(a[index])
			b_digit := if borrow { u64(1) } else { u64(0) }
			borrow = a_digit < b_digit
			if borrow {
				a_digit += 0x100000000
			}
			a[index] = u32(a_digit - b_digit)
		}
	} else { // if len.b > len.a return zero
		a.clear()
	}
}
