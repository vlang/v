module big

import strings

@[direct_array_access; inline]
fn shrink_tail_zeros(mut a []u64) {
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

// debug_u64_str output a `[]u64`
@[direct_array_access]
fn debug_u64_str(a []u64) string {
	mut sb := strings.new_builder(30)
	sb.write_string('[')
	mut first := true
	for i in 0 .. a.len {
		if !first {
			sb.write_string(', ')
		}
		sb.write_string('0x${a[i].hex():016}')
		first = false
	}
	sb.write_string(']')
	return sb.str()
}

// debug_u32_str for 32bit bignum test only, convert a `[]u64` to `[]u32`.
@[direct_array_access]
fn debug_u32_str(a []u64) string {
	mut b := []u32{cap: a.len * 2}
	mut curr_u32 := u32(0)
	mut bits_collected := 0
	for w in a {
		for i in 0 .. digit_bits {
			bit := (w >> i) & 1
			curr_u32 |= u32(bit) << bits_collected
			bits_collected++
			if bits_collected == 32 {
				b << curr_u32
				curr_u32 = 0
				bits_collected = 0
			}
		}
	}
	if bits_collected > 0 {
		b << curr_u32
	}

	mut blen := b.len
	for blen > 0 && b[blen - 1] == 0 {
		blen--
	}
	unsafe {
		b.len = blen
	}
	mut sb := strings.new_builder(30)
	sb.write_string('[')
	mut first := true
	for i in 0 .. b.len {
		if !first {
			sb.write_string(', ')
		}
		sb.write_string('0x${b[i].hex():08}')
		first = false
	}
	sb.write_string(']')
	return sb.str()
}

@[direct_array_access; inline]
fn found_multiplication_base_case(operand_a []u64, operand_b []u64, mut storage []u64) bool {
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
fn karatsuba_multiply_digit_array(operand_a []u64, operand_b []u64, mut storage []u64) {
	if found_multiplication_base_case(operand_a, operand_b, mut storage) {
		return
	}

	// thanks to the base cases we can pass zero-length arrays to the mult func
	half := imax(operand_a.len, operand_b.len) / 2
	mut a_l := unsafe { operand_a[0..half] }
	mut a_h := unsafe { operand_a[half..] }
	mut b_l := []u64{}
	mut b_h := []u64{}
	if half <= operand_b.len {
		b_l = unsafe { operand_b[0..half] }
		b_h = unsafe { operand_b[half..] }
	} else {
		b_l = unsafe { operand_b }
		// b_h = []u64{}
	}
	shrink_tail_zeros(mut a_l)
	shrink_tail_zeros(mut a_h)
	shrink_tail_zeros(mut b_l)
	shrink_tail_zeros(mut b_h)

	// use storage for p_1 to avoid allocation and copy later
	multiply_digit_array(a_h, b_h, mut storage)

	mut p_3 := []u64{len: a_l.len + b_l.len + 1}
	multiply_digit_array(a_l, b_l, mut p_3)

	mut tmp_1 := []u64{len: imax(a_h.len, a_l.len) + 1}
	mut tmp_2 := []u64{len: imax(b_h.len, b_l.len) + 1}
	add_digit_array(a_h, a_l, mut tmp_1)
	add_digit_array(b_h, b_l, mut tmp_2)

	mut p_2 := []u64{len: operand_a.len + operand_b.len + 1}
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
fn toom3_multiply_digit_array(operand_a []u64, operand_b []u64, mut storage []u64) {
	if found_multiplication_base_case(operand_a, operand_b, mut storage) {
		return
	}

	// After the base case, we have operand_a as the larger integer in terms of digit length

	// k is the length (in u64 digits) of the lower order slices
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
				digits: operand_b[..k].clone()
				signum: 1
			}
		}
		b0.shrink_tail_zeros()
		b1 = Integer{
			digits: operand_b[k..].clone()
			signum: 1
		}
	} else {
		if !operand_b[..k].all(it == 0) {
			b0 = Integer{
				digits: operand_b[..k].clone()
				signum: 1
			}
		}
		b0.shrink_tail_zeros()
		if !operand_b[k..k2].all(it == 0) {
			b1 = Integer{
				digits: operand_b[k..k2].clone()
				signum: 1
			}
		}
		b1.shrink_tail_zeros()
		b2 = Integer{
			digits: operand_b[k2..].clone()
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
	s := u32(k) * digit_bits

	result := (((pinf.left_shift(s) + t2).left_shift(s) + t1).left_shift(s) + tm1).left_shift(s) +
		p0

	storage = result.digits.clone()
}

@[inline]
fn pow2(k int) Integer {
	mut ret := []u64{len: (k / digit_bits) + 1}
	bit_set(mut ret, k)
	return Integer{
		signum: 1
		digits: ret
	}
}

// optimized left shift in place. amount must be positive
fn left_shift_digits_in_place(mut a []u64, amount int) {
	// this is actual in builtin/array.v, prepend_many (private fn)
	// x := []u64{ len : amount }
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
fn right_shift_digits_in_place(mut a []u64, amount int) {
	a.drop(amount)
}

// operand b can be greater than operand a
// the capacity of both array is supposed to be sufficient
@[direct_array_access; inline]
fn add_in_place(mut a []u64, b []u64) {
	len_a := a.len
	len_b := b.len
	max := imax(len_a, len_b)
	min := imin(len_a, len_b)
	mut carry := u64(0)
	for index in 0 .. min {
		partial := carry + a[index] + b[index]
		a[index] = u64(partial) & max_digit
		carry = u64(partial >> digit_bits)
	}
	if len_a >= len_b {
		for index in min .. max {
			partial := carry + a[index]
			a[index] = u64(partial) & max_digit
			carry = u64(partial >> digit_bits)
		}
	} else {
		for index in min .. max {
			partial := carry + b[index]
			a << u64(partial) & max_digit
			carry = u64(partial >> digit_bits)
		}
	}
	if carry > 0 {
		a << carry
	}
}

// a := a - b supposed a >= b
@[direct_array_access; inline]
fn subtract_in_place(mut a []u64, b []u64) {
	len_a := a.len
	len_b := b.len
	max := imax(len_a, len_b)
	min := imin(len_a, len_b)

	mut borrow := false
	for index in 0 .. min {
		mut a_digit := a[index]
		b_digit := b[index] + if borrow { u64(1) } else { u64(0) }
		borrow = a_digit < b_digit
		if borrow {
			a_digit = a_digit | (u64(1) << digit_bits)
		}
		a[index] = a_digit - b_digit
	}

	if len_a >= len_b {
		for index in min .. max {
			mut a_digit := a[index]
			b_digit := if borrow { u64(1) } else { u64(0) }
			borrow = a_digit < b_digit
			if borrow {
				a_digit = a_digit | (u64(1) << digit_bits)
			}
			a[index] = a_digit - b_digit
		}
	} else { // if len.b > len.a return zero
		a.clear()
	}
}
