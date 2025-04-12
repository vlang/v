module builtin

pub type byte = u8

// type i32 = int

// digit pairs in reverse order
const digit_pairs = '00102030405060708090011121314151617181910212223242526272829203132333435363738393041424344454647484940515253545556575859506162636465666768696071727374757677787970818283848586878889809192939495969798999'

pub const min_i8 = i8(-128)
pub const max_i8 = i8(127)

pub const min_i16 = i16(-32768)
pub const max_i16 = i16(32767)

pub const min_i32 = i32(-2147483648)
pub const max_i32 = i32(2147483647)

pub const min_int = min_i32
pub const max_int = max_i32

// -9223372036854775808 is wrong, because C compilers parse literal values
// without sign first, and 9223372036854775808 overflows i64, hence the
// consecutive subtraction by 1
pub const min_i64 = i64(-9223372036854775807 - 1)
pub const max_i64 = i64(9223372036854775807)

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

		// return tos(memdup(&buf[0] + index, (max - index)), (max - index))
	}
}

// str returns the value of the `int_literal` as a `string`.
@[inline]
pub fn (n int_literal) str() string {
	return i64(n).str()
}

// str returns the value of the `i64` as a `string`.
// Example: assert i64(-200000).str() == '-200000'
@[direct_array_access; inline]
pub fn (nn i64) str() string {
	unsafe {
		mut n := nn
		mut d := i64(0)
		if n == 0 {
			return '0'
		} else if n == min_i64 {
			// math.min_i64
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
		// return tos(memdup(&buf[0] + index, (max - index)), (max - index))
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
		// return tos(memdup(&buf[0] + index, (max - index)), (max - index))
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
