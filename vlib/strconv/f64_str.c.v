module strconv

/*=============================================================================

f64 to string

Copyright (c) 2019-2022 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.

This file contains the f64 to string functions

These functions are based on the work of:
Publication:PLDI 2018: Proceedings of the 39th ACM SIGPLAN
Conference on Programming Language Design and ImplementationJune 2018
Pages 270–282 https://doi.org/10.1145/3192366.3192369

inspired by the Go version here:
https://github.com/cespare/ryu/tree/ba56a33f39e3bbbfa409095d0f9ae168a595feea

=============================================================================*/

[direct_array_access]
fn (d Dec64) get_string_64(neg bool, i_n_digit int, i_pad_digit int) string {
	mut n_digit := i_n_digit + 1
	pad_digit := i_pad_digit + 1
	mut out := d.m
	mut d_exp := d.e
	// mut out_len      := decimal_len_64(out)
	mut out_len := dec_digits(out)
	out_len_original := out_len

	mut fw_zeros := 0
	if pad_digit > out_len {
		fw_zeros = pad_digit - out_len
	}

	mut buf := []u8{len: (out_len + 6 + 1 + 1 + fw_zeros)} // sign + mant_len + . +  e + e_sign + exp_len(2) + \0}
	mut i := 0

	if neg {
		buf[i] = `-`
		i++
	}

	mut disp := 0
	if out_len <= 1 {
		disp = 1
	}

	// rounding last used digit
	if n_digit < out_len {
		// println("out:[$out]")
		out += ten_pow_table_64[out_len - n_digit - 1] * 5 // round to up
		out /= ten_pow_table_64[out_len - n_digit]
		// println("out1:[$out] ${d.m / ten_pow_table_64[out_len - n_digit ]}")
		if d.m / ten_pow_table_64[out_len - n_digit] < out {
			d_exp++
			n_digit++
		}

		// println("cmp: ${d.m/ten_pow_table_64[out_len - n_digit ]} ${out/ten_pow_table_64[out_len - n_digit ]}")

		out_len = n_digit
		// println("orig: ${out_len_original} new len: ${out_len} out:[$out]")
	}

	y := i + out_len
	mut x := 0
	for x < (out_len - disp - 1) {
		buf[y - x] = `0` + u8(out % 10)
		out /= 10
		i++
		x++
	}

	// no decimal digits needed, end here
	if i_n_digit == 0 {
		unsafe {
			buf[i] = 0
			return tos(&u8(&buf[0]), i)
		}
	}

	if out_len >= 1 {
		buf[y - x] = `.`
		x++
		i++
	}

	if y - x >= 0 {
		buf[y - x] = `0` + u8(out % 10)
		i++
	}

	for fw_zeros > 0 {
		buf[i] = `0`
		i++
		fw_zeros--
	}

	buf[i] = `e`
	i++

	mut exp := d_exp + out_len_original - 1
	if exp < 0 {
		buf[i] = `-`
		i++
		exp = -exp
	} else {
		buf[i] = `+`
		i++
	}

	// Always print at least two digits to match strconv's formatting.
	d2 := exp % 10
	exp /= 10
	d1 := exp % 10
	d0 := exp / 10
	if d0 > 0 {
		buf[i] = `0` + u8(d0)
		i++
	}
	buf[i] = `0` + u8(d1)
	i++
	buf[i] = `0` + u8(d2)
	i++
	buf[i] = 0

	return unsafe {
		tos(&u8(&buf[0]), i)
	}
}

fn f64_to_decimal_exact_int(i_mant u64, exp u64) (Dec64, bool) {
	mut d := Dec64{}
	e := exp - bias64
	if e > mantbits64 {
		return d, false
	}
	shift := mantbits64 - e
	mant := i_mant | u64(0x0010_0000_0000_0000) // implicit 1
	// mant  := i_mant | (1 << mantbits64) // implicit 1
	d.m = mant >> shift
	if (d.m << shift) != mant {
		return d, false
	}

	for (d.m % 10) == 0 {
		d.m /= 10
		d.e++
	}
	return d, true
}

fn f64_to_decimal(mant u64, exp u64) Dec64 {
	mut e2 := 0
	mut m2 := u64(0)
	if exp == 0 {
		// We subtract 2 so that the bounds computation has
		// 2 additional bits.
		e2 = 1 - bias64 - int(mantbits64) - 2
		m2 = mant
	} else {
		e2 = int(exp) - bias64 - int(mantbits64) - 2
		m2 = (u64(1) << mantbits64) | mant
	}
	even := (m2 & 1) == 0
	accept_bounds := even

	// Step 2: Determine the interval of valid decimal representations.
	mv := u64(4 * m2)
	mm_shift := bool_to_u64(mant != 0 || exp <= 1)

	// Step 3: Convert to a decimal power base uing 128-bit arithmetic.
	mut vr := u64(0)
	mut vp := u64(0)
	mut vm := u64(0)
	mut e10 := 0
	mut vm_is_trailing_zeros := false
	mut vr_is_trailing_zeros := false

	if e2 >= 0 {
		// This expression is slightly faster than max(0, log10Pow2(e2) - 1).
		q := log10_pow2(e2) - bool_to_u32(e2 > 3)
		e10 = int(q)
		k := pow5_inv_num_bits_64 + pow5_bits(int(q)) - 1
		i := -e2 + int(q) + k

		mul := pow5_inv_split_64[q]
		vr = mul_shift_64(u64(4) * m2, mul, i)
		vp = mul_shift_64(u64(4) * m2 + u64(2), mul, i)
		vm = mul_shift_64(u64(4) * m2 - u64(1) - mm_shift, mul, i)
		if q <= 21 {
			// This should use q <= 22, but I think 21 is also safe.
			// Smaller values may still be safe, but it's more
			// difficult to reason about them. Only one of mp, mv,
			// and mm can be a multiple of 5, if any.
			if mv % 5 == 0 {
				vr_is_trailing_zeros = multiple_of_power_of_five_64(mv, q)
			} else if accept_bounds {
				// Same as min(e2 + (^mm & 1), pow5Factor64(mm)) >= q
				// <=> e2 + (^mm & 1) >= q && pow5Factor64(mm) >= q
				// <=> true && pow5Factor64(mm) >= q, since e2 >= q.
				vm_is_trailing_zeros = multiple_of_power_of_five_64(mv - 1 - mm_shift,
					q)
			} else if multiple_of_power_of_five_64(mv + 2, q) {
				vp--
			}
		}
	} else {
		// This expression is slightly faster than max(0, log10Pow5(-e2) - 1).
		q := log10_pow5(-e2) - bool_to_u32(-e2 > 1)
		e10 = int(q) + e2
		i := -e2 - int(q)
		k := pow5_bits(i) - pow5_num_bits_64
		j := int(q) - k
		mul := pow5_split_64[i]
		vr = mul_shift_64(u64(4) * m2, mul, j)
		vp = mul_shift_64(u64(4) * m2 + u64(2), mul, j)
		vm = mul_shift_64(u64(4) * m2 - u64(1) - mm_shift, mul, j)
		if q <= 1 {
			// {vr,vp,vm} is trailing zeros if {mv,mp,mm} has at least q trailing 0 bits.
			// mv = 4 * m2, so it always has at least two trailing 0 bits.
			vr_is_trailing_zeros = true
			if accept_bounds {
				// mm = mv - 1 - mmShift, so it has 1 trailing 0 bit iff mmShift == 1.
				vm_is_trailing_zeros = (mm_shift == 1)
			} else {
				// mp = mv + 2, so it always has at least one trailing 0 bit.
				vp--
			}
		} else if q < 63 { // TODO(ulfjack/cespare): Use a tighter bound here.
			// We need to compute min(ntz(mv), pow5Factor64(mv) - e2) >= q - 1
			// <=> ntz(mv) >= q - 1 && pow5Factor64(mv) - e2 >= q - 1
			// <=> ntz(mv) >= q - 1 (e2 is negative and -e2 >= q)
			// <=> (mv & ((1 << (q - 1)) - 1)) == 0
			// We also need to make sure that the left shift does not overflow.
			vr_is_trailing_zeros = multiple_of_power_of_two_64(mv, q - 1)
		}
	}

	// Step 4: Find the shortest decimal representation
	// in the interval of valid representations.
	mut removed := 0
	mut last_removed_digit := u8(0)
	mut out := u64(0)
	// On average, we remove ~2 digits.
	if vm_is_trailing_zeros || vr_is_trailing_zeros {
		// General case, which happens rarely (~0.7%).
		for {
			vp_div_10 := vp / 10
			vm_div_10 := vm / 10
			if vp_div_10 <= vm_div_10 {
				break
			}
			vm_mod_10 := vm % 10
			vr_div_10 := vr / 10
			vr_mod_10 := vr % 10
			vm_is_trailing_zeros = vm_is_trailing_zeros && vm_mod_10 == 0
			vr_is_trailing_zeros = vr_is_trailing_zeros && (last_removed_digit == 0)
			last_removed_digit = u8(vr_mod_10)
			vr = vr_div_10
			vp = vp_div_10
			vm = vm_div_10
			removed++
		}
		if vm_is_trailing_zeros {
			for {
				vm_div_10 := vm / 10
				vm_mod_10 := vm % 10
				if vm_mod_10 != 0 {
					break
				}
				vp_div_10 := vp / 10
				vr_div_10 := vr / 10
				vr_mod_10 := vr % 10
				vr_is_trailing_zeros = vr_is_trailing_zeros && (last_removed_digit == 0)
				last_removed_digit = u8(vr_mod_10)
				vr = vr_div_10
				vp = vp_div_10
				vm = vm_div_10
				removed++
			}
		}
		if vr_is_trailing_zeros && (last_removed_digit == 5) && (vr % 2) == 0 {
			// Round even if the exact number is .....50..0.
			last_removed_digit = 4
		}
		out = vr
		// We need to take vr + 1 if vr is outside bounds
		// or we need to round up.
		if (vr == vm && (!accept_bounds || !vm_is_trailing_zeros)) || last_removed_digit >= 5 {
			out++
		}
	} else {
		// Specialized for the common case (~99.3%).
		// Percentages below are relative to this.
		mut round_up := false
		for vp / 100 > vm / 100 {
			// Optimization: remove two digits at a time (~86.2%).
			round_up = (vr % 100) >= 50
			vr /= 100
			vp /= 100
			vm /= 100
			removed += 2
		}
		// Loop iterations below (approximately), without optimization above:
		// 0: 0.03%, 1: 13.8%, 2: 70.6%, 3: 14.0%, 4: 1.40%, 5: 0.14%, 6+: 0.02%
		// Loop iterations below (approximately), with optimization above:
		// 0: 70.6%, 1: 27.8%, 2: 1.40%, 3: 0.14%, 4+: 0.02%
		for vp / 10 > vm / 10 {
			round_up = (vr % 10) >= 5
			vr /= 10
			vp /= 10
			vm /= 10
			removed++
		}
		// We need to take vr + 1 if vr is outside bounds
		// or we need to round up.
		out = vr + bool_to_u64(vr == vm || round_up)
	}

	return Dec64{
		m: out
		e: e10 + removed
	}
}

//=============================================================================
// String Functions
//=============================================================================

// f64_to_str returns `f` as a `string` in scientific notation with max `n_digit` digits after the dot.
pub fn f64_to_str(f f64, n_digit int) string {
	mut u1 := Uf64{}
	u1.f = f
	u := unsafe { u1.u }

	neg := (u >> (mantbits64 + expbits64)) != 0
	mant := u & ((u64(1) << mantbits64) - u64(1))
	exp := (u >> mantbits64) & ((u64(1) << expbits64) - u64(1))
	// println("s:${neg} mant:${mant} exp:${exp} float:${f} byte:${u1.u:016lx}")

	// Exit early for easy cases.
	if (exp == maxexp64) || (exp == 0 && mant == 0) {
		return get_string_special(neg, exp == 0, mant == 0)
	}

	mut d, ok := f64_to_decimal_exact_int(mant, exp)
	if !ok {
		// println("to_decimal")
		d = f64_to_decimal(mant, exp)
	}
	// println("${d.m} ${d.e}")
	return d.get_string_64(neg, n_digit, 0)
}

// f64_to_str returns `f` as a `string` in scientific notation with max `n_digit` digits after the dot.
pub fn f64_to_str_pad(f f64, n_digit int) string {
	mut u1 := Uf64{}
	u1.f = f
	u := unsafe { u1.u }

	neg := (u >> (mantbits64 + expbits64)) != 0
	mant := u & ((u64(1) << mantbits64) - u64(1))
	exp := (u >> mantbits64) & ((u64(1) << expbits64) - u64(1))
	// println("s:${neg} mant:${mant} exp:${exp} float:${f} byte:${u1.u:016lx}")

	// Exit early for easy cases.
	if (exp == maxexp64) || (exp == 0 && mant == 0) {
		return get_string_special(neg, exp == 0, mant == 0)
	}

	mut d, ok := f64_to_decimal_exact_int(mant, exp)
	if !ok {
		// println("to_decimal")
		d = f64_to_decimal(mant, exp)
	}
	// println("DEBUG: ${d.m} ${d.e}")
	return d.get_string_64(neg, n_digit, n_digit)
}
