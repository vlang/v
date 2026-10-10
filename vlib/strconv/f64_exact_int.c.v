module strconv

// A finite f64 with an unbiased exponent of 52 or more is an integer: its 53 bit mantissa
// shifted left. Such a value can have up to 309 decimal digits, while the shortest decimal
// that converts back to it has 17 at most. The functions below produce all of them.

// the limbs of the decimal integer hold 9 digits each
const exact_int_base = u64(1_000_000_000)
// the largest f64 is below 2^1024, which has 309 digits: 35 limbs
const exact_int_max_limbs = 35

// f64_is_exact_int returns true when `u` holds the bits of a finite f64 of 2^52 or more
// in magnitude.
@[inline]
fn f64_is_exact_int(u u64) bool {
	exp := int((u >> mantbits64) & maxexp64)
	return exp >= bias64 + int(mantbits64) && exp != int(maxexp64)
}

// f64_exact_int_to_str returns the exact decimal digits of the f64 with the bits `u`, for which
// `f64_is_exact_int` must be true, followed by a dot and `dec_digit` zeros when `dec_digit`
// is positive.
@[direct_array_access]
fn f64_exact_int_to_str(u u64, dec_digit int) string {
	neg := (u >> (mantbits64 + expbits64)) != 0
	exp := int((u >> mantbits64) & maxexp64)
	mut mant := (u & ((u64(1) << mantbits64) - u64(1))) | (u64(1) << mantbits64)

	// little endian limbs of `mant`
	mut limbs := [exact_int_max_limbs]u32{}
	mut n := 0
	for mant > 0 {
		limbs[n] = u32(mant % exact_int_base)
		mant /= exact_int_base
		n++
	}
	// Multiply by 2^shift, at most 32 bits at a time: a limb is below 2^30 and the carry
	// below 2^33, so that a shifted limb plus the carry fits in 64 bits.
	mut shift := exp - bias64 - int(mantbits64)
	for shift > 0 {
		step := if shift > 32 { 32 } else { shift }
		mut carry := u64(0)
		for i in 0 .. n {
			v := (u64(limbs[i]) << step) + carry
			carry = v / exact_int_base
			limbs[i] = u32(v - carry * exact_int_base)
		}
		for carry > 0 {
			limbs[n] = u32(carry % exact_int_base)
			carry /= exact_int_base
			n++
		}
		shift -= step
	}

	sign_len := if neg { 1 } else { 0 }
	top_len := dec_digits(u64(limbs[n - 1]))
	int_end := sign_len + top_len + (n - 1) * 9
	res_len := int_end + if dec_digit > 0 { dec_digit + 1 } else { 0 }
	// The digits are written straight into the buffer of the returned string,
	// which makes this the only allocation.
	unsafe {
		mut buf := malloc_noscan(res_len + 1)
		if neg {
			buf[0] = `-`
		}
		mut pos := int_end
		for i in 0 .. n {
			mut limb := limbs[i]
			limb_len := if i == n - 1 { top_len } else { 9 }
			for _ in 0 .. limb_len {
				pos--
				buf[pos] = u8(limb % 10) + `0`
				limb /= 10
			}
		}
		if dec_digit > 0 {
			buf[int_end] = `.`
			for i in int_end + 1 .. res_len {
				buf[i] = `0`
			}
		}
		buf[res_len] = 0
		return tos(buf, res_len)
	}
}
