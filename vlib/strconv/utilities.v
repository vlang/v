module strconv

import math.bits

// general utilities

// General Utilities
@[if debug_strconv ?]
fn assert1(t bool, msg string) {
	if !t {
		panic(msg)
	}
}

@[inline]
fn bool_to_int(b bool) int {
	if b {
		return 1
	}
	return 0
}

@[inline]
fn bool_to_u32(b bool) u32 {
	if b {
		return u32(1)
	}
	return u32(0)
}

@[inline]
fn bool_to_u64(b bool) u64 {
	if b {
		return u64(1)
	}
	return u64(0)
}

fn get_string_special(neg bool, expZero bool, mantZero bool) string {
	if !mantZero {
		return 'nan'
	}
	if !expZero {
		if neg {
			return '-inf'
		} else {
			return '+inf'
		}
	}
	if neg {
		return '-0e+00'
	}
	return '0e+00'
}

/*
32 bit functions
*/

fn mul_shift_32(m u32, mul u64, ishift int) u32 {
	// QTODO
	// assert ishift > 32

	hi, lo := bits.mul_64(u64(m), mul)
	shifted_sum := (lo >> u64(ishift)) + (hi << u64(64 - ishift))
	assert1(shifted_sum <= 2147483647, 'shiftedSum <= math.max_u32')
	return u32(shifted_sum)
}

@[direct_array_access; inline]
fn mul_pow5_invdiv_pow2(m u32, q u32, j int) u32 {
	assert1(q < pow5_inv_split_32.len, 'q < pow5_inv_split_32.len')
	return mul_shift_32(m, pow5_inv_split_32[q], j)
}

@[direct_array_access; inline]
fn mul_pow5_div_pow2(m u32, i u32, j int) u32 {
	assert1(i < pow5_split_32.len, 'i < pow5_split_32.len')
	return mul_shift_32(m, pow5_split_32[i], j)
}

fn pow5_factor_32(i_v u32) u32 {
	mut v := i_v
	for n := u32(0); true; n++ {
		q := v / 5
		r := v % 5
		if r != 0 {
			return n
		}
		v = q
	}
	return v
}

// multiple_of_power_of_five_32 reports whether v is divisible by 5^p.
fn multiple_of_power_of_five_32(v u32, p u32) bool {
	return pow5_factor_32(v) >= p
}

// multiple_of_power_of_two_32 reports whether v is divisible by 2^p.
fn multiple_of_power_of_two_32(v u32, p u32) bool {
	return u32(bits.trailing_zeros_32(v)) >= p
}

// log10_pow2 returns floor(log_10(2^e)).
fn log10_pow2(e int) u32 {
	// The first value this approximation fails for is 2^1651
	// which is just greater than 10^297.
	assert1(e >= 0, 'e >= 0')
	assert1(e <= 1650, 'e <= 1650')
	return (u32(e) * 78913) >> 18
}

// log10_pow5 returns floor(log_10(5^e)).
fn log10_pow5(e int) u32 {
	// The first value this approximation fails for is 5^2621
	// which is just greater than 10^1832.
	assert1(e >= 0, 'e >= 0')
	assert1(e <= 2620, 'e <= 2620')
	return (u32(e) * 732923) >> 20
}

// pow5_bits returns ceil(log_2(5^e)), or else 1 if e==0.
fn pow5_bits(e int) int {
	// This approximation works up to the point that the multiplication
	// overflows at e = 3529. If the multiplication were done in 64 bits,
	// it would fail at 5^4004 which is just greater than 2^9297.
	assert1(e >= 0, 'e >= 0')
	assert1(e <= 3528, 'e <= 3528')
	return int(((u32(e) * 1217359) >> 19) + 1)
}

/*
64 bit functions
*/

fn shift_right_128(v Uint128, shift int) u64 {
	// The shift value is always modulo 64.
	// In the current implementation of the 64-bit version
	// of Ryu, the shift value is always < 64.
	// (It is in the range [2, 59].)
	// Check this here in case a future change requires larger shift
	// values. In this case this function needs to be adjusted.
	assert1(shift < 64, 'shift < 64')
	return (v.hi << u64(64 - shift)) | (v.lo >> u32(shift))
}

fn mul_shift_64(m u64, mul Uint128, shift int) u64 {
	hihi, hilo := bits.mul_64(m, mul.hi)
	lohi, _ := bits.mul_64(m, mul.lo)
	mut sum := Uint128{
		lo: lohi + hilo
		hi: hihi
	}
	if sum.lo < lohi {
		sum.hi++ // overflow
	}
	return shift_right_128(sum, shift - 64)
}

fn pow5_factor_64(v_i u64) u32 {
	mut v := v_i
	for n := u32(0); true; n++ {
		q := v / 5
		r := v % 5
		if r != 0 {
			return n
		}
		v = q
	}
	return u32(0)
}

fn multiple_of_power_of_five_64(v u64, p u32) bool {
	return pow5_factor_64(v) >= p
}

fn multiple_of_power_of_two_64(v u64, p u32) bool {
	return u32(bits.trailing_zeros_64(v)) >= p
}

// dec_digits return the number of decimal digit of an u64
pub fn dec_digits(n u64) int {
	if n <= 9_999_999_999 { // 1-10
		if n <= 99_999 { // 5
			if n <= 99 { // 2
				if n <= 9 { // 1
					return 1
				} else {
					return 2
				}
			} else {
				if n <= 999 { // 3
					return 3
				} else {
					if n <= 9999 { // 4
						return 4
					} else {
						return 5
					}
				}
			}
		} else {
			if n <= 9_999_999 { // 7
				if n <= 999_999 { // 6
					return 6
				} else {
					return 7
				}
			} else {
				if n <= 99_999_999 { // 8
					return 8
				} else {
					if n <= 999_999_999 { // 9
						return 9
					}
					return 10
				}
			}
		}
	} else {
		if n <= 999_999_999_999_999 { // 5
			if n <= 999_999_999_999 { // 2
				if n <= 99_999_999_999 { // 1
					return 11
				} else {
					return 12
				}
			} else {
				if n <= 9_999_999_999_999 { // 3
					return 13
				} else {
					if n <= 99_999_999_999_999 { // 4
						return 14
					} else {
						return 15
					}
				}
			}
		} else {
			if n <= 99_999_999_999_999_999 { // 7
				if n <= 9_999_999_999_999_999 { // 6
					return 16
				} else {
					return 17
				}
			} else {
				if n <= 999_999_999_999_999_999 { // 8
					return 18
				} else {
					if n <= 9_999_999_999_999_999_999 { // 9
						return 19
					}
					return 20
				}
			}
		}
	}
}
