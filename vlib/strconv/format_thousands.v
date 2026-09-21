module strconv

import strings

// Separator configures the separators used by numeric formatting functions.
// `integer` is inserted every three digits in the integer part, while `decimal`
// replaces the decimal point before the fractional part. An empty `decimal`
// keeps the default `.` decimal point.
pub struct Separator {
pub:
	// integer is inserted every three digits in the integer part.
	integer string = ' '
	// decimal separates the integer and fractional parts.
	decimal string = '.'
}

pub type SeparatorOptions = string | Separator

// is_all_digits reports whether every byte in `s` is an ASCII digit (`0`-`9`).
// An empty string reports `true` (vacuously), matching how `insert_thousands_sep`
// already treats `''` as nothing to group.
fn is_all_digits(s string) bool {
	for b in s {
		if b < `0` || b > `9` {
			return false
		}
	}
	return true
}

// insert_thousands_sep inserts `sep` into the ASCII-digit string `digits`
// every three digits, counting from the right. `digits` must not contain a
// sign or a decimal point; use `add_thousands_sep` for full numeric strings.
//
// Non-finite float representations (`inf`, `-inf`, `nan`, `Infinity`,
// `-Infinity`, `NaN`, etc., depending on the backend) reach here as their
// integer part with no digits at all, or with letters mixed in; `digits` is
// returned unchanged whenever it isn't purely numeric, instead of being
// sliced into arbitrary chunks.
fn insert_thousands_sep(digits string, sep string) string {
	if digits.len <= 3 || sep == '' || !is_all_digits(digits) {
		return digits
	}

	mut sb := strings.new_builder(digits.len + (digits.len / 3) * sep.len)

	for i := 0; i < digits.len; i++ {
		if i > 0 && (digits.len - i) % 3 == 0 {
			sb.write_string(sep)
		}
		sb.write_byte(digits[i])
	}

	return sb.str()
}

// expand_exponent rewrites a decimal string in scientific notation (e.g.
// `'1e+21'`, `'1.5e-07'`, `'-2.5E10'`) into an equivalent plain decimal
// string with no exponent. A string without an exponent is returned
// unchanged. This is a pure, backend-independent string transformation (it
// only shifts the decimal point; it never re-parses or rounds the
// underlying value), so it produces fixed-point output on backends whose
// native float-to-string conversion switches to scientific notation for
// large or small magnitudes (currently the JS backend; see
// `vlib/builtin/js/float.js.v`, which does `x.val + ''`).
fn expand_exponent(s string) string {
	mut e_idx := -1
	for i := 0; i < s.len; i++ {
		if s[i] == `e` || s[i] == `E` {
			e_idx = i
			break
		}
	}
	if e_idx == -1 {
		return s
	}

	mut sign := ''
	mut mantissa := s[..e_idx]
	if mantissa.len > 0 && (mantissa[0] == `-` || mantissa[0] == `+`) {
		sign = mantissa[..1]
		mantissa = mantissa[1..]
	}

	mut exp_str := s[e_idx + 1..]
	mut exp_sign := 1
	if exp_str.len > 0 && (exp_str[0] == `-` || exp_str[0] == `+`) {
		if exp_str[0] == `-` {
			exp_sign = -1
		}
		exp_str = exp_str[1..]
	}
	exponent := exp_sign * exp_str.int()

	dot := mantissa.index_u8(`.`)
	mut int_digits := mantissa
	mut frac_digits := ''
	if dot != -1 {
		int_digits = mantissa[..dot]
		frac_digits = mantissa[dot + 1..]
	}

	all_digits := int_digits + frac_digits
	if !is_all_digits(all_digits) {
		// Not actually numeric (shouldn't happen for a real float.str()
		// output) - don't try to shift a decimal point through it.
		return s
	}

	point_pos := int_digits.len + exponent

	result := if point_pos <= 0 {
		'0.' + strings.repeat(`0`, -point_pos) + all_digits
	} else if point_pos >= all_digits.len {
		all_digits + strings.repeat(`0`, point_pos - all_digits.len) + '.0'
	} else {
		all_digits[..point_pos] + '.' + all_digits[point_pos..]
	}

	return sign + result
}

// add_thousands_sep inserts `sep` as a thousands separator into the
// integer part of the numeric string `s`. An optional leading sign (`-` or `+`)
// is preserved, and fractional digits are kept unchanged except that their
// decimal separator is replaced by `sep.decimal` when `sep` is a `Separator`.
// A string `sep` uses that string for the integer separator and keeps `.` as
// the decimal separator.
//
// Because it operates on an already-formatted string, it composes with any
// number-to-string conversion: `int`/`i64`/`u64.str()`, `f64.str()`,
// `strconv.ftoa_*`, `math.big.Integer.str()`, etc.
//
// This function does not do full locale-aware formatting: it groups only the
// digits before the input decimal point and does not parse or round the value.
//
// Example:
// assert strconv.add_thousands_sep('1234567', Separator{}) == '1 234 567'
// assert strconv.add_thousands_sep('-1234567.89', Separator{
// 	integer: ',',
// 	decimal: ',',
// }) == '-1,234,567,89'
pub fn add_thousands_sep(s string, sep SeparatorOptions) string {
	if s == '' {
		return s
	}

	separator := match sep {
		string {
			Separator{
				integer: sep
				decimal: '.'
			}
		}
		Separator {
			sep
		}
	}

	mut sign := ''
	mut rest := s

	if rest[0] == `-` || rest[0] == `+` {
		sign = rest[..1]
		rest = rest[1..]
	}

	mut exponent := ''
	for i := 0; i < rest.len; i++ {
		if rest[i] == `e` || rest[i] == `E` {
			exponent = rest[i..]
			rest = rest[..i]
			break
		}
	}

	dot := rest.index_u8(`.`)
	mut int_part := rest
	mut frac_part := ''

	if dot != -1 {
		int_part = rest[..dot]
		frac_part = rest[dot + 1..]

		if separator.decimal != '' {
			frac_part = separator.decimal + frac_part
		} else {
			frac_part = '.' + frac_part
		}
	}

	return sign + insert_thousands_sep(int_part, separator.integer) + frac_part + exponent
}

// format_thousands returns the base-10 representation of `number`, inserting
// a thousands separator every three digits. With a `Separator`, its `integer`
// and `decimal` fields configure the output; a string uses that string for
// grouping and `.` as the decimal separator. It accepts all built-in signed
// and unsigned integer types, `isize`, `usize`, `f32`, and `f64`.
//
// Example:
// assert strconv.format_thousands(1234567, Separator{}) == '1 234 567'
// assert strconv.format_thousands(-1234567, Separator{integer: ','}) == '-1,234,567'
// assert strconv.format_thousands(1234567.89, Separator{
// 	integer: '.',
// 	decimal: ',',
// }) == '1.234.567,89'
pub fn format_thousands[T](number T, sep SeparatorOptions) string {
	$if T !in [int, i8, i16, i32, i64, u8, u16, u32, u64, isize, usize, f32, f64] {
		$compile_error('format_thousands() expects a numeric type')
	}

	separator := match sep {
		string {
			Separator{
				integer: sep
				decimal: '.'
			}
		}
		Separator {
			sep
		}
	}

	$if js {
		$if T is f32 || T is f64 {
			// `float.str()` on the JS backend does `x.val + ''`, which -
			// like plain JS number-to-string conversion - switches to
			// scientific notation for large/small magnitudes (e.g. `1e21`
			// becomes `'1e+21'`). Expand it back to fixed-point first so
			// grouping still applies to the full integer part.
			return add_thousands_sep(expand_exponent(number.str()), separator)
		} $else {
			return add_thousands_sep(number.str(), separator)
		}
	} $else $if T is f64 {
		return add_thousands_sep(f64_to_str_l_with_dot(number), separator)
	} $else $if T is f32 {
		return add_thousands_sep(f32_to_str_l(number), separator)
	} $else {
		return add_thousands_sep(number.str(), separator)
	}
}
