module json5

import math
import strconv

// Null is the JSON5 `null` literal.
pub struct Null {
}

// str returns `Null` rendered as JSON5 text.
pub fn (n Null) str() string {
	return 'null'
}

// Number is a JSON5 number. The literal source text is preserved so that
// `0x1F`, `+1`, `.5`, `5.`, `Infinity` and `NaN` can be written back out
// unchanged.
pub struct Number {
pub:
	text string // literal source text, for example `0x1F` or `-Infinity`
}

// str returns the number as its original literal text.
pub fn (n Number) str() string {
	return n.text
}

// i64 returns the number as an i64. Hexadecimal and decimal integers convert
// exactly; fractional and non-finite values truncate toward zero.
pub fn (n Number) i64() i64 {
	if v := n.try_int() {
		return v
	}
	f := n.f64()
	if f > 9_223_372_036_854_775_807.0 {
		return 9223372036854775807
	}
	if f < -9_223_372_036_854_775_808.0 {
		return -9223372036854775807 - 1
	}
	return i64(f)
}

// int returns the number as an int.
pub fn (n Number) int() int {
	return int(n.i64())
}

// u64 returns the number as a u64. Negative values clamp to zero.
pub fn (n Number) u64() u64 {
	if v := n.try_int() {
		if v < 0 {
			return u64(0)
		}
		return u64(v)
	}
	f := n.f64()
	if f <= 0.0 {
		return u64(0)
	}
	return u64(f)
}

// f64 returns the number as an f64. `Infinity` and `NaN` keep their IEEE value.
pub fn (n Number) f64() f64 {
	text := n.text
	negative := text.starts_with('-')
	digits := text.trim_left('+-')
	if digits == 'Infinity' {
		bits := if negative { u64(0xFFF0000000000000) } else { u64(0x7FF0000000000000) }
		return unsafe { math.f64_from_bits(bits) }
	}
	if digits == 'NaN' {
		return unsafe { math.f64_from_bits(u64(0x7FF8000000000000)) }
	}
	if digits.starts_with('0x') || digits.starts_with('0X') {
		return if negative { -f64(hex_value(digits[2..])) } else { f64(hex_value(digits[2..])) }
	}
	// V rejects `+` in numeric literals, and accepts `.5` / `5.` only after a
	// digit has been seen, so normalize to plain `strconv`-friendly text.
	normalized := normalize_float_text(digits)
	value := strconv.atof64(normalized, strconv.AtoF64Param{}) or { return 0.0 }
	return if negative { -value } else { value }
}

// hex_value returns the integer value of a hexadecimal digit string.
fn hex_value(digits string) u64 {
	mut value := u64(0)
	for ch in digits.runes() {
		value = value << 4 | u64(hex_digit_value(int(ch)))
	}
	return value
}

// try_int returns the exact integer value of `n`, or none when `n` is
// fractional or non-finite.
fn (n Number) try_int() ?i64 {
	text := n.text
	digits := text.trim_left('+-')
	if digits.contains('.') || digits.contains('e') || digits.contains('E') {
		return none
	}
	if digits.starts_with('0x') || digits.starts_with('0X') {
		value := hex_value(digits[2..])
		// `i64(value)` is a reinterpretation of the bits, so it only gives the
		// intended negative value below 2^63.
		if text.starts_with('-') && value > 1 << 63 {
			return none
		}
		return i64(value)
	}
	parsed := strconv.parse_int(text, 10, 64) or { return none }
	return parsed
}

// normalize_float_text turns JSON5 float spellings into text `strconv.atof64`
// accepts: `.5` becomes `0.5`, `5.` becomes `5`, and a bare exponent such as
// `1e3` gains a fraction so that no integer path is taken.
fn normalize_float_text(digits string) string {
	mut out := digits
	if out.starts_with('.') {
		out = '0' + out
	} else if out.ends_with('.') {
		out += '0'
	}
	// A lone exponent such as `1e3` is a valid JSON5 float; `strconv` rejects it,
	// so append a fraction to keep the value in the floating-point path.
	if !out.contains('.') {
		idx := out.index_any('eE')
		if idx > 0 {
			out = out[..idx] + '.0' + out[idx..]
		}
	}
	return out
}
