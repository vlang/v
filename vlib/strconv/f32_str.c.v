module strconv

/*=============================================================================

f32 to string

Copyright (c) 2019-2024 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.

This file contains the f32 to string functions

These functions are based on the work of:
Publication:PLDI 2018: Proceedings of the 39th ACM SIGPLAN
Conference on Programming Language Design and ImplementationJune 2018
Pages 270–282 https://doi.org/10.1145/3192366.3192369

inspired by the Go version here:
https://github.com/cespare/ryu/tree/ba56a33f39e3bbbfa409095d0f9ae168a595feea

=============================================================================*/

// pow of ten table used by n_digit reduction
const ten_pow_table_32 = [
	u32(1),
	u32(10),
	u32(100),
	u32(1000),
	u32(10000),
	u32(100000),
	u32(1000000),
	u32(10000000),
	u32(100000000),
	u32(1000000000),
	u32(10000000000),
	u32(100000000000),
]!

//=============================================================================
// Conversion Functions
//=============================================================================
const mantbits32 = u32(23)
const expbits32 = u32(8)
const bias32 = 127 // f32 exponent bias

const maxexp32 = 255

// max 46 char
// -3.40282346638528859811704183484516925440e+38
@[direct_array_access]
pub fn (d Dec32) get_string_32(neg bool, i_n_digit int, i_pad_digit int) string {
	n_digit := i_n_digit + 1
	pad_digit := i_pad_digit + 1
	mut out := d.m
	// mut out_len      := decimal_len_32(out)
	mut out_len := dec_digits(out)
	out_len_original := out_len

	mut fw_zeros := 0
	if pad_digit > out_len {
		fw_zeros = pad_digit - out_len
	}

	mut buf := []u8{len: int(out_len + 5 + 1 + 1)} // sign + mant_len + . +  e + e_sign + exp_len(2) + \0}
	mut i := 0

	if neg {
		if buf.data != 0 {
			// The buf.data != 0 check here, is needed for clean compilation
			// with `-cc gcc -cstrict -prod`. Without it, gcc produces:
			// error: potential null pointer dereference
			buf[i] = `-`
		}
		i++
	}

	mut disp := 0
	if out_len <= 1 {
		disp = 1
	}

	if n_digit < out_len {
		// println("orig: ${out_len_original}")
		out += strconv.ten_pow_table_32[out_len - n_digit - 1] * 5 // round to up
		out /= strconv.ten_pow_table_32[out_len - n_digit]
		out_len = n_digit
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

	mut exp := d.e + out_len_original - 1
	if exp < 0 {
		buf[i] = `-`
		i++
		exp = -exp
	} else {
		buf[i] = `+`
		i++
	}

	// Always print two digits to match strconv's formatting.
	d1 := exp % 10
	d0 := exp / 10
	buf[i] = `0` + u8(d0)
	i++
	buf[i] = `0` + u8(d1)
	i++
	buf[i] = 0

	return unsafe {
		tos(&u8(&buf[0]), i)
	}
}

fn f32_to_decimal_exact_int(i_mant u32, exp u32) (Dec32, bool) {
	mut d := Dec32{}
	e := exp - strconv.bias32
	if e > strconv.mantbits32 {
		return d, false
	}
	shift := strconv.mantbits32 - e
	mant := i_mant | 0x0080_0000 // implicit 1
	// mant := i_mant | (1 << mantbits32) // implicit 1
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

fn f32_to_decimal(mant u32, exp u32) Dec32 {
	mut e2 := 0
	mut m2 := u32(0)
	if exp == 0 {
		// We subtract 2 so that the bounds computation has
		// 2 additional bits.
		e2 = 1 - strconv.bias32 - int(strconv.mantbits32) - 2
		m2 = mant
	} else {
		e2 = int(exp) - strconv.bias32 - int(strconv.mantbits32) - 2
		m2 = (u32(1) << strconv.mantbits32) | mant
	}
	even := (m2 & 1) == 0
	accept_bounds := even

	// Step 2: Determine the interval of valid decimal representations.
	mv := u32(4 * m2)
	mp := u32(4 * m2 + 2)
	mm_shift := bool_to_u32(mant != 0 || exp <= 1)
	mm := u32(4 * m2 - 1 - mm_shift)

	mut vr := u32(0)
	mut vp := u32(0)
	mut vm := u32(0)
	mut e10 := 0
	mut vm_is_trailing_zeros := false
	mut vr_is_trailing_zeros := false
	mut last_removed_digit := u8(0)

	if e2 >= 0 {
		q := log10_pow2(e2)
		e10 = int(q)
		k := pow5_inv_num_bits_32 + pow5_bits(int(q)) - 1
		i := -e2 + int(q) + k

		vr = mul_pow5_invdiv_pow2(mv, q, i)
		vp = mul_pow5_invdiv_pow2(mp, q, i)
		vm = mul_pow5_invdiv_pow2(mm, q, i)
		if q != 0 && (vp - 1) / 10 <= vm / 10 {
			// We need to know one removed digit even if we are not
			// going to loop below. We could use q = X - 1 above,
			// except that would require 33 bits for the result, and
			// we've found that 32-bit arithmetic is faster even on
			// 64-bit machines.
			l := pow5_inv_num_bits_32 + pow5_bits(int(q - 1)) - 1
			last_removed_digit = u8(mul_pow5_invdiv_pow2(mv, q - 1, -e2 + int(q - 1) + l) % 10)
		}
		if q <= 9 {
			// The largest power of 5 that fits in 24 bits is 5^10,
			// but q <= 9 seems to be safe as well. Only one of mp,
			// mv, and mm can be a multiple of 5, if any.
			if mv % 5 == 0 {
				vr_is_trailing_zeros = multiple_of_power_of_five_32(mv, q)
			} else if accept_bounds {
				vm_is_trailing_zeros = multiple_of_power_of_five_32(mm, q)
			} else if multiple_of_power_of_five_32(mp, q) {
				vp--
			}
		}
	} else {
		q := log10_pow5(-e2)
		e10 = int(q) + e2
		i := -e2 - int(q)
		k := pow5_bits(i) - pow5_num_bits_32
		mut j := int(q) - k
		vr = mul_pow5_div_pow2(mv, u32(i), j)
		vp = mul_pow5_div_pow2(mp, u32(i), j)
		vm = mul_pow5_div_pow2(mm, u32(i), j)
		if q != 0 && ((vp - 1) / 10) <= vm / 10 {
			j = int(q) - 1 - (pow5_bits(i + 1) - pow5_num_bits_32)
			last_removed_digit = u8(mul_pow5_div_pow2(mv, u32(i + 1), j) % 10)
		}
		if q <= 1 {
			// {vr,vp,vm} is trailing zeros if {mv,mp,mm} has at
			// least q trailing 0 bits. mv = 4 * m2, so it always
			// has at least two trailing 0 bits.
			vr_is_trailing_zeros = true
			if accept_bounds {
				// mm = mv - 1 - mm_shift, so it has 1 trailing 0 bit
				// if mm_shift == 1.
				vm_is_trailing_zeros = mm_shift == 1
			} else {
				// mp = mv + 2, so it always has at least one
				// trailing 0 bit.
				vp--
			}
		} else if q < 31 {
			vr_is_trailing_zeros = multiple_of_power_of_two_32(mv, q - 1)
		}
	}

	// Step 4: Find the shortest decimal representation
	// in the interval of valid representations.
	mut removed := 0
	mut out := u32(0)
	if vm_is_trailing_zeros || vr_is_trailing_zeros {
		// General case, which happens rarely (~4.0%).
		for vp / 10 > vm / 10 {
			vm_is_trailing_zeros = vm_is_trailing_zeros && (vm % 10) == 0
			vr_is_trailing_zeros = vr_is_trailing_zeros && last_removed_digit == 0
			last_removed_digit = u8(vr % 10)
			vr /= 10
			vp /= 10
			vm /= 10
			removed++
		}
		if vm_is_trailing_zeros {
			for vm % 10 == 0 {
				vr_is_trailing_zeros = vr_is_trailing_zeros && last_removed_digit == 0
				last_removed_digit = u8(vr % 10)
				vr /= 10
				vp /= 10
				vm /= 10
				removed++
			}
		}
		if vr_is_trailing_zeros && last_removed_digit == 5 && (vr % 2) == 0 {
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
		// Specialized for the common case (~96.0%). Percentages below
		// are relative to this. Loop iterations below (approximately):
		// 0: 13.6%, 1: 70.7%, 2: 14.1%, 3: 1.39%, 4: 0.14%, 5+: 0.01%
		for vp / 10 > vm / 10 {
			last_removed_digit = u8(vr % 10)
			vr /= 10
			vp /= 10
			vm /= 10
			removed++
		}
		// We need to take vr + 1 if vr is outside bounds
		// or we need to round up.
		out = vr + bool_to_u32(vr == vm || last_removed_digit >= 5)
	}

	return Dec32{
		m: out
		e: e10 + removed
	}
}

//=============================================================================
// String Functions
//=============================================================================

// f32_to_str returns a `string` in scientific notation with max `n_digit` after the dot.
pub fn f32_to_str(f f32, n_digit int) string {
	mut u1 := Uf32{}
	u1.f = f
	u := unsafe { u1.u }

	neg := (u >> (strconv.mantbits32 + strconv.expbits32)) != 0
	mant := u & ((u32(1) << strconv.mantbits32) - u32(1))
	exp := (u >> strconv.mantbits32) & ((u32(1) << strconv.expbits32) - u32(1))

	// println("${neg} ${mant} e ${exp-bias32}")

	// Exit early for easy cases.
	if exp == strconv.maxexp32 || (exp == 0 && mant == 0) {
		return get_string_special(neg, exp == 0, mant == 0)
	}

	mut d, ok := f32_to_decimal_exact_int(mant, exp)
	if !ok {
		// println("with exp form")
		d = f32_to_decimal(mant, exp)
	}

	// println("${d.m} ${d.e}")
	return d.get_string_32(neg, n_digit, 0)
}

// f32_to_str_pad returns a `string` in scientific notation with max `n_digit` after the dot.
pub fn f32_to_str_pad(f f32, n_digit int) string {
	mut u1 := Uf32{}
	u1.f = f
	u := unsafe { u1.u }

	neg := (u >> (strconv.mantbits32 + strconv.expbits32)) != 0
	mant := u & ((u32(1) << strconv.mantbits32) - u32(1))
	exp := (u >> strconv.mantbits32) & ((u32(1) << strconv.expbits32) - u32(1))

	// println("${neg} ${mant} e ${exp-bias32}")

	// Exit early for easy cases.
	if exp == strconv.maxexp32 || (exp == 0 && mant == 0) {
		return get_string_special(neg, exp == 0, mant == 0)
	}

	mut d, ok := f32_to_decimal_exact_int(mant, exp)
	if !ok {
		// println("with exp form")
		d = f32_to_decimal(mant, exp)
	}

	// println("${d.m} ${d.e}")
	return d.get_string_32(neg, n_digit, n_digit)
}
