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

// insert_thousands_sep inserts `sep` into the ASCII-digit string `digits`
// every three digits, counting from the right. `digits` must not contain a
// sign or a decimal point; use `add_thousands_sep` for full numeric strings.
fn insert_thousands_sep(digits string, sep string) string {
	if digits.len <= 3 || sep == '' {
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
		return add_thousands_sep(number.str(), separator)
	} $else $if T is f64 {
		return add_thousands_sep(f64_to_str_l_with_dot(number), separator)
	} $else $if T is f32 {
		return add_thousands_sep(f32_to_str_l(number), separator)
	} $else {
		return add_thousands_sep(number.str(), separator)
	}
}
