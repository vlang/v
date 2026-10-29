module decimal

import math.big

// from_json_number implements a custom decoder for json2.
// It parses `raw_number` like `from_string`, so the digits after the decimal
// point set the scale. A JSON5 hexadecimal integer such as `0x1F` or `-0x1F`
// has scale 0.
pub fn (mut d Decimal) from_json_number(raw_number string) ! {
	sign_len := if raw_number.starts_with('-') || raw_number.starts_with('+') { 1 } else { 0 }
	if raw_number.len > sign_len + 1 && raw_number[sign_len] == `0`
		&& raw_number[sign_len + 1] in [`x`, `X`] {
		hex_digits := raw_number[sign_len + 2..]
		if hex_digits.len == 0 || !hex_digits.bytes().all(it.is_hex_digit()) {
			return error('math.decimal: invalid decimal value `${raw_number}`')
		}
		d = new(big.integer_from_radix(raw_number[..sign_len] + hex_digits, 16)!, 0)
		return
	}
	d = from_string(raw_number)!
}

// from_json_string implements a custom decoder for json2, for values sent as
// JSON strings such as `"19.99"`. `raw_string` must have the syntax of a JSON
// number: no surrounding whitespace, no leading `+` sign, and digits on both sides of
// a decimal point.
pub fn (mut d Decimal) from_json_string(raw_string string) ! {
	if !is_json_number(raw_string) {
		return error('math.decimal: invalid JSON number `${raw_string}`')
	}
	d.from_json_number(raw_string)!
}

// to_json implements a custom encoder for json2. It writes `d` as a JSON number
// in plain decimal notation and keeps the stored scale, so `10.50` encodes as
// `10.50` and decodes back to the same coefficient and scale.
pub fn (d Decimal) to_json() string {
	return d.format_decimal()
}

// is_json_number reports whether `s` matches the JSON number grammar:
// `-? (0 | [1-9][0-9]*) (. [0-9]+)? ([eE] [+-]? [0-9]+)?`.
fn is_json_number(s string) bool {
	mut i := 0
	if i < s.len && s[i] == `-` {
		i++
	}
	if i >= s.len || !s[i].is_digit() {
		return false
	}
	if s[i] == `0` {
		i++
	} else {
		for i < s.len && s[i].is_digit() {
			i++
		}
	}
	if i < s.len && s[i] == `.` {
		i++
		start := i
		for i < s.len && s[i].is_digit() {
			i++
		}
		if i == start {
			return false
		}
	}
	if i < s.len && (s[i] == `e` || s[i] == `E`) {
		i++
		if i < s.len && (s[i] == `+` || s[i] == `-`) {
			i++
		}
		start := i
		for i < s.len && s[i].is_digit() {
			i++
		}
		if i == start {
			return false
		}
	}
	return i == s.len
}
