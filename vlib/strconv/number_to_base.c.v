module strconv

const base_digits = '0123456789abcdefghijklmnopqrstuvwxyz'

// format_int returns the string representation of the number n in base `radix`
// Digits with values >= 10 use the lowercase Latin letters a-z.
@[direct_array_access; manualfree]
pub fn format_int(n i64, radix int) string {
	unsafe {
		if radix < 2 || radix > 36 {
			panic_n('invalid radix, it should be => 2 and <= 36, actual:', radix)
		}
		if n == 0 {
			return '0'
		}
		// A 64-bit value needs at most 64 digits (base 2), plus room for a sign
		// and the terminating zero byte. Fill the buffer from the end in one
		// pass and copy it out with a single allocation, instead of prepending
		// one digit at a time, which allocated and copied O(digits^2) bytes.
		mut buf := malloc_noscan(66)
		mut index := 65
		buf[index] = 0
		index--
		mut n_copy := u64(n)
		if n < 0 {
			// Avoid negating min_i64, whose magnitude only fits in u64.
			n_copy = u64(-(n + 1)) + 1
		}
		uradix := u64(radix)
		for n_copy != 0 {
			buf[index] = base_digits[int(n_copy % uradix)]
			index--
			n_copy /= uradix
		}
		if n < 0 {
			buf[index] = `-`
			index--
		}
		index++
		diff := 65 - index
		vmemmove(buf, voidptr(buf + index), diff + 1)
		return tos(buf, diff)
	}
}

// format_uint returns the string representation of the number n in base `radix`
// for digit values > 10, this function uses the small latin leters a-z.
@[direct_array_access; manualfree]
pub fn format_uint(n u64, radix int) string {
	unsafe {
		if radix < 2 || radix > 36 {
			panic_n('invalid radix, it should be => 2 and <= 36, actual:', radix)
		}
		if n == 0 {
			return '0'
		}
		// See format_int: fill from the end and allocate once, rather than
		// prepending one digit at a time in O(digits^2) time and allocations.
		mut buf := malloc_noscan(66)
		mut index := 65
		buf[index] = 0
		index--
		mut n_copy := n
		uradix := u64(radix)
		for n_copy != 0 {
			buf[index] = base_digits[int(n_copy % uradix)]
			index--
			n_copy /= uradix
		}
		index++
		diff := 65 - index
		vmemmove(buf, voidptr(buf + index), diff + 1)
		return tos(buf, diff)
	}
}
