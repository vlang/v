module strconv

const base_digits = '0123456789abcdefghijklmnopqrstuvwxyz'

// format_int returns the string representation of the number n in base `radix`
// for digit values > 10, this function uses the small latin leters a-z.
[direct_array_access; manualfree]
pub fn format_int(n i64, radix int) string {
	unsafe {
		if radix < 2 || radix > 36 {
			panic('invalid radix: $radix . It should be => 2 and <= 36')
		}
		if n == 0 {
			return '0'
		}
		mut n_copy := n
		mut have_minus := false
		if n < 0 {
			have_minus = true
			n_copy = -n_copy
		}
		mut res := ''
		for n_copy != 0 {
			tmp_0 := res
			bdx := int(n_copy % radix)
			tmp_1 := strconv.base_digits[bdx].ascii_str()
			res = tmp_1 + res
			tmp_0.free()
			tmp_1.free()
			// res = base_digits[n_copy % radix].ascii_str() + res
			n_copy /= radix
		}
		if have_minus {
			final_res := '-' + res
			res.free()
			return final_res
		}
		return res
	}
}

// format_uint returns the string representation of the number n in base `radix`
// for digit values > 10, this function uses the small latin leters a-z.
[direct_array_access; manualfree]
pub fn format_uint(n u64, radix int) string {
	unsafe {
		if radix < 2 || radix > 36 {
			panic('invalid radix: $radix . It should be => 2 and <= 36')
		}
		if n == 0 {
			return '0'
		}
		mut n_copy := n
		mut res := ''
		uradix := u64(radix)
		for n_copy != 0 {
			tmp_0 := res
			tmp_1 := strconv.base_digits[n_copy % uradix].ascii_str()
			res = tmp_1 + res
			tmp_0.free()
			tmp_1.free()
			// res = base_digits[n_copy % uradix].ascii_str() + res
			n_copy /= uradix
		}
		return res
	}
}
