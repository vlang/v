module builtin

pub type byte = u8

// str returns the string equivalent of x.
pub fn (x isize) str() string {
	return i64(x).str()
}

// str returns the string equivalent of x.
pub fn (x usize) str() string {
	return u64(x).str()
}

// digit pairs in reverse order
const digit_pairs = '00102030405060708090011121314151617181910212223242526272829203132333435363738393041424344454647484940515253545556575859506162636465666768696071727374757677787970818283848586878889809192939495969798999'

pub const min_i8 = i8(-128)
pub const max_i8 = i8(127)

pub const min_i16 = i16(-32768)
pub const max_i16 = i16(32767)

pub const min_i32 = i32(-2147483648)
pub const max_i32 = i32(2147483647)

// -9223372036854775808 is wrong, because C compilers parse literal values
// without sign first, and 9223372036854775808 overflows i64, hence the
// consecutive subtraction by 1
pub const min_i64 = i64(-9223372036854775807 - 1)
pub const max_i64 = i64(9223372036854775807)

pub const min_int = $if new_int ? && x64 { int(min_i64) } $else { int(min_i32) }
pub const max_int = $if new_int ? && x64 { int(max_i64) } $else { int(max_i32) }

pub const min_u8 = u8(0)
pub const max_u8 = u8(255)

pub const min_u16 = u16(0)
pub const max_u16 = u16(65535)

pub const min_u32 = u32(0)
pub const max_u32 = u32(4294967295)

pub const min_u64 = u64(0)
pub const max_u64 = u64(18446744073709551615)

// This implementation is the quickest with gcc -O2
// str_l returns the string representation of the integer nn with max chars.
@[direct_array_access; inline]
fn (nn int) str_l(max int) string {
	unsafe {
		mut n := i64(nn)
		mut d := 0
		if n == 0 {
			return '0'
		}

		mut is_neg := false
		if n < 0 {
			n = -n
			is_neg = true
		}
		mut index := max
		mut buf := malloc(max + 1)
		buf[index] = 0
		index--

		for n > 0 {
			n1 := int(n / 100)
			// calculate the digit_pairs start index
			d = int(u32(int(n) - (n1 * 100)) << 1)
			n = n1
			buf[index] = digit_pairs.str[d]
			index--
			d++
			buf[index] = digit_pairs.str[d]
			index--
		}
		index++
		// remove head zero
		if d < 20 {
			index++
		}
		// Prepend - if it's negative
		if is_neg {
			index--
			buf[index] = `-`
		}
		diff := max - index
		vmemmove(buf, voidptr(buf + index), diff + 1)
		return tos(buf, diff)

		// return tos(memdup(&buf[0] + index, (max - index)), (max - index))
	}
}

// str returns the value of the `u8` as a `string`.
// Example: assert u8(2).str() == '2'
pub fn (n u8) str() string {
	return int(n).str_l(5)
}

// str returns the value of the `i8` as a `string`.
// Example: assert i8(-2).str() == '-2'
pub fn (n i8) str() string {
	return int(n).str_l(5)
}

// str returns the value of the `i16` as a `string`.
// Example: assert i16(-20).str() == '-20'
pub fn (n i16) str() string {
	return int(n).str_l(7)
}

// str returns the value of the `u16` as a `string`.
// Example: assert u16(20).str() == '20'
pub fn (n u16) str() string {
	return int(n).str_l(7)
}

pub fn (n i32) str() string {
	return int(n).str_l(12)
}

// str returns the value of the `int` as a `string`.
// Example: assert int(-2020).str() == '-2020'
pub fn (n int) str() string {
	return n.str_l(12)
}

// str returns the value of the `u32` as a `string`.
// Example: assert u32(20000).str() == '20000'
@[direct_array_access; inline]
pub fn (nn u32) str() string {
	unsafe {
		mut n := nn
		mut d := u32(0)
		if n == 0 {
			return '0'
		}
		max := 12
		mut buf := malloc(max + 1)
		mut index := max
		buf[index] = 0
		index--
		for n > 0 {
			n1 := n / u32(100)
			d = ((n - (n1 * u32(100))) << u32(1))
			n = n1
			buf[index] = digit_pairs[d]
			index--
			d++
			buf[index] = digit_pairs[d]
			index--
		}
		index++
		// remove head zero
		if d < u32(20) {
			index++
		}
		diff := max - index
		vmemmove(buf, voidptr(buf + index), diff + 1)
		return tos(buf, diff)
	}
}

// str returns the value of the `int_literal` as a `string`.
@[inline]
pub fn (n int_literal) str() string {
	return impl_i64_to_string(n)
}

// str returns the value of the `i64` as a `string`.
// Example: assert i64(-200000).str() == '-200000'
@[inline]
pub fn (nn i64) str() string {
	return impl_i64_to_string(nn)
}

@[direct_array_access]
fn impl_i64_to_string(nn i64) string {
	unsafe {
		mut n := nn
		mut d := i64(0)
		if n == 0 {
			return '0'
		} else if n == min_i64 {
			return '-9223372036854775808'
		}
		max := 20
		mut buf := malloc(max + 1)
		mut is_neg := false
		if n < 0 {
			n = -n
			is_neg = true
		}
		mut index := max
		buf[index] = 0
		index--
		for n > 0 {
			n1 := n / i64(100)
			d = (u32(n - (n1 * i64(100))) << i64(1))
			n = n1
			buf[index] = digit_pairs[d]
			index--
			d++
			buf[index] = digit_pairs[d]
			index--
		}
		index++
		// remove head zero
		if d < i64(20) {
			index++
		}
		// Prepend - if it's negative
		if is_neg {
			index--
			buf[index] = `-`
		}
		diff := max - index
		vmemmove(buf, voidptr(buf + index), diff + 1)
		return tos(buf, diff)
	}
}

// str returns the value of the `u64` as a `string`.
// Example: assert u64(2000000).str() == '2000000'
@[direct_array_access; inline]
pub fn (nn u64) str() string {
	unsafe {
		mut n := nn
		mut d := u64(0)
		if n == 0 {
			return '0'
		}
		max := 20
		mut buf := malloc(max + 1)
		mut index := max
		buf[index] = 0
		index--
		for n > 0 {
			n1 := n / 100
			d = ((n - (n1 * 100)) << 1)
			n = n1
			buf[index] = digit_pairs[d]
			index--
			d++
			buf[index] = digit_pairs[d]
			index--
		}
		index++
		// remove head zero
		if d < 20 {
			index++
		}
		diff := max - index
		vmemmove(buf, voidptr(buf + index), diff + 1)
		return tos(buf, diff)
	}
}

// str returns the value of the `bool` as a `string`.
// Example: assert (2 > 1).str() == 'true'
pub fn (b bool) str() string {
	if b {
		return 'true'
	}
	return 'false'
}

pub fn (p voidptr) str() string {
	return u64(p).str()
}

pub fn (x f32) str() string {
	return f64(x).str()
}

pub fn (x f64) str() string {
	// Format f64 as string similar to the native C version
	// Maximum 6 significant decimal places
	unsafe {
		if x != x {
			return 'nan'
		}
		if x > 1.7976931348623157e+308 {
			return 'inf'
		}
		if x < -1.7976931348623157e+308 {
			return '-inf'
		}

		mut is_neg := false
		mut val := x
		if val < 0 {
			is_neg = true
			val = -val
		}

		// Round to 6 decimal places to handle floating point precision
		val = (val * 1000000.0 + 0.5) / 1000000.0

		mut int_part := i64(val)
		mut frac_part := val - f64(int_part)

		// If no fractional part or very close to zero, return just the integer
		if frac_part < 1e-10 {
			result := int_part.str()
			if is_neg {
				// Prepend negative sign manually
				mut buf := malloc(result.len + 2)
				buf[0] = `-`
				vmemcpy(buf + 1, result.str, result.len)
				buf[result.len + 1] = 0
				return tos(buf, result.len + 1)
			}
			return result
		}

		// Build fractional part - up to 6 decimal places
		mut frac_digits := u64(0)
		mut frac_count := 0
		mut temp_frac := frac_part

		for frac_count < 6 {
			temp_frac *= 10.0
			digit := u64(temp_frac)
			frac_digits = frac_digits * 10 + digit
			temp_frac -= f64(digit)
			frac_count++
			if temp_frac < 1e-10 {
				break
			}
		}

		// Remove trailing zeros
		for frac_count > 0 && frac_digits % 10 == 0 {
			frac_digits /= 10
			frac_count--
		}

		if frac_count == 0 {
			result := int_part.str()
			if is_neg {
				// Prepend negative sign manually
				mut buf := malloc(result.len + 2)
				buf[0] = `-`
				vmemcpy(buf + 1, result.str, result.len)
				buf[result.len + 1] = 0
				return tos(buf, result.len + 1)
			}
			return result
		}

		// Allocate string buffer: worst case is "-123456789012345.123456"
		max_len := 30
		buf := malloc(max_len + 1)
		mut pos := 0

		// Copy negative sign if needed
		if is_neg {
			buf[pos] = `-`
			pos++
		}

		// Copy integer part using tos to get the string bytes
		int_str := int_part.str()
		vmemcpy(buf + pos, int_str.str, int_str.len)
		pos += int_str.len

		// Add decimal point
		buf[pos] = `.`
		pos++

		// Format fractional digits
		frac_str := frac_digits.str()
		// Pad with leading zeros if needed
		for _ in 0 .. (frac_count - frac_str.len) {
			buf[pos] = `0`
			pos++
		}
		// Copy fractional string bytes
		vmemcpy(buf + pos, frac_str.str, frac_str.len)
		pos += frac_str.len

		buf[pos] = 0
		return tos(buf, pos)
	}
}
