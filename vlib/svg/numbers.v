module svg

import math
import strconv

// NumberScan holds a parsed leading number and how many bytes it consumed.
pub struct NumberScan {
pub:
	value    f64
	consumed int
}

// scan_number reads a leading SVG number (sign, decimal, exponent) from s.
// It returns none when s does not start with a valid number.
pub fn scan_number(s string, start int) ?NumberScan {
	mut i := start
	if i >= s.len {
		return none
	}
	if s[i] == `+` || s[i] == `-` {
		i++
	}
	mut digits := 0
	for i < s.len && s[i] >= `0` && s[i] <= `9` {
		i++
		digits++
	}
	if i < s.len && s[i] == `.` {
		i++
		for i < s.len && s[i] >= `0` && s[i] <= `9` {
			i++
			digits++
		}
	}
	if digits == 0 {
		return none
	}
	if i < s.len && (s[i] == `e` || s[i] == `E`) {
		mut j := i + 1
		if j < s.len && (s[j] == `+` || s[j] == `-`) {
			j++
		}
		mut exp_digits := 0
		for j < s.len && s[j] >= `0` && s[j] <= `9` {
			j++
			exp_digits++
		}
		if exp_digits > 0 {
			i = j
		}
	}
	value := strconv.atof64(s[start..i]) or { return none }
	return NumberScan{
		value:    value
		consumed: i - start
	}
}

// parse_number parses a whole string as an SVG number.
pub fn parse_number(s string) !f64 {
	scan := scan_number(s, 0) or { return error('invalid SVG number: ${s}') }
	if scan.consumed != s.len {
		return error('trailing data in SVG number: ${s}')
	}
	return scan.value
}

// skip_ws skips SVG whitespace at position i and returns the next index.
pub fn skip_ws(s string, i int) int {
	mut p := i
	for p < s.len && (s[p] == ` ` || s[p] == `\t` || s[p] == `\n` || s[p] == `\r`) {
		p++
	}
	return p
}

// fmt_num formats a float for SVG output: short, deterministic, no long tails.
pub fn fmt_num(v f64) string {
	if v == 0 || math.is_nan(v) {
		return '0'
	}
	if math.is_inf(v, 1) {
		return '1e15'
	}
	if math.is_inf(v, -1) {
		return '-1e15'
	}
	// Round to at most 6 decimal places to avoid f32 binary noise.
	rounded := math.round(v * 1_000_000) / 1_000_000
	if rounded == 0 {
		return '0'
	}
	return rounded.str()
}

// fmt_f32 formats an f32 coordinate for SVG output.
pub fn fmt_f32(v f32) string {
	return fmt_num(f64(v))
}

// clamp_limits clamps a value into the inclusive range [lo, hi].
pub fn clamp_limits(v f64, lo f64, hi f64) f64 {
	if v < lo {
		return lo
	}
	if v > hi {
		return hi
	}
	return v
}
