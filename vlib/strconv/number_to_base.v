module strconv

const base_digits = '0123456789abcdefghijklmnopqrstuvwxyz'

// format_int returns the string representation of the number n in base `radix`
// for digit values > 10, this function uses the small latin leters a-z.
pub fn format_int(n i64, radix int) string {
	if radix < 2 || radix > 36 {
		panic('invalid radix: $radix . It should be => 2 and <= 36')
	}
	if n == 0 {
		return '0'
	}
	mut n_copy := n
	mut sign := ''
	if n < 0 {
		sign = '-'
		n_copy = -n_copy
	}
	mut res := ''
	for n_copy != 0 {
		res = base_digits[n_copy % radix].ascii_str() + res
		n_copy /= radix
	}
	return '$sign$res'
}

// format_uint returns the string representation of the number n in base `radix`
// for digit values > 10, this function uses the small latin leters a-z.
pub fn format_uint(n u64, radix int) string {
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
		res = base_digits[n_copy % uradix].ascii_str() + res
		n_copy /= uradix
	}
	return res
}
