module kdl

import math.big
import strconv
import strings

// Character classes and low-level scanning for KDL 2.0.
// The whole input is validated as UTF-8 up front (see validate_input), so the
// scanner can decode runes without further checks.

// is_newline_rune reports whether r is a KDL newline (spec 3.18).
@[inline]
fn is_newline_rune(r rune) bool {
	return r == 0x0A || r == 0x0D || r == 0x0B || r == 0x0C || r == 0x85 || r == 0x2028
		|| r == 0x2029
}

// is_space_rune reports whether r is KDL whitespace that is not a newline (spec 3.17).
@[inline]
fn is_space_rune(r rune) bool {
	return r == 0x09 || r == 0x20 || r == 0xA0 || r == 0x1680 || (r >= 0x2000 && r <= 0x200A)
		|| r == 0x202F || r == 0x205F || r == 0x3000
}

// is_ident_byte reports whether an ASCII byte may appear in an identifier string.
@[inline]
fn is_ident_byte(b u8) bool {
	if b <= 0x20 || b == 0x7F {
		return false
	}
	return b !in [u8(`\\`), `/`, `(`, `)`, `{`, `}`, `;`, `[`, `]`, `"`, `#`, `=`]!
}

// is_disallowed_rune reports whether r may never appear literally (spec 3.19).
// U+FEFF is only allowed as the very first code point and is handled by the caller.
fn is_disallowed_rune(r rune) bool {
	if r <= 0x08 || (r >= 0x0E && r <= 0x1F) || r == 0x7F {
		return true
	}
	if r == 0x200E || r == 0x200F || (r >= 0x202A && r <= 0x202E) || (r >= 0x2066 && r <= 0x2069) {
		return true
	}
	return r == 0xFEFF
}

// decode_rune decodes the UTF-8 sequence at s[i]. Input must be valid UTF-8.
@[direct_array_access; inline]
fn decode_rune(s string, i int) (rune, int) {
	b := s[i]
	if b < 0x80 {
		return rune(b), 1
	}
	if b < 0xE0 {
		return (rune(b & 0x1F) << 6) | rune(s[i + 1] & 0x3F), 2
	}
	if b < 0xF0 {
		return (rune(b & 0x0F) << 12) | (rune(s[i + 1] & 0x3F) << 6) | rune(s[i + 2] & 0x3F), 3
	}
	return (rune(b & 0x07) << 18) | (rune(s[i + 1] & 0x3F) << 12) | (rune(s[i + 2] & 0x3F) << 6) | rune(s[i + 3] & 0x3F), 4
}

// validate_input checks that s[start..] is well-formed UTF-8 and contains no
// disallowed literal code point. Returns the byte offset of the first problem.
@[direct_array_access]
fn validate_input(s string, start int) !int {
	mut i := start
	n := s.len
	for i < n {
		b := s[i]
		if b < 0x80 {
			if (b < 0x20 && b != 0x09 && b != 0x0A && b != 0x0B && b != 0x0C && b != 0x0D)
				|| b == 0x7F {
				return error_at(i, 'control character U+${u32(b):04X} is not allowed literally')
			}
			i++
			continue
		}
		mut cp := u32(0)
		mut len := 0
		if b >= 0xC2 && b <= 0xDF {
			len = 2
			cp = u32(b & 0x1F)
		} else if b >= 0xE0 && b <= 0xEF {
			len = 3
			cp = u32(b & 0x0F)
		} else if b >= 0xF0 && b <= 0xF4 {
			len = 4
			cp = u32(b & 0x07)
		} else {
			return error_at(i, 'invalid UTF-8 byte 0x${b:02X}')
		}
		if i + len > n {
			return error_at(i, 'truncated UTF-8 sequence')
		}
		for k in 1 .. len {
			c := s[i + k]
			if c & 0xC0 != 0x80 {
				return error_at(i, 'invalid UTF-8 continuation byte')
			}
			cp = (cp << 6) | u32(c & 0x3F)
		}
		if (len == 3 && cp < 0x800) || (len == 4 && (cp < 0x10000 || cp > 0x10FFFF)) {
			return error_at(i, 'invalid UTF-8 sequence')
		}
		if cp >= 0xD800 && cp <= 0xDFFF {
			return error_at(i, 'UTF-8 encoded surrogate U+${u32(cp):04X} is not allowed')
		}
		if is_disallowed_rune(rune(cp)) {
			return error_at(i, 'code point U+${u32(cp):04X} is not allowed literally')
		}
		i += len
	}
	return n
}

// offset_error carries a byte offset; the parser turns it into a ParseError.
struct OffsetError {
	Error
	offset  int
	message string
}

fn (e OffsetError) msg() string {
	return e.message
}

fn error_at(offset int, message string) IError {
	return OffsetError{
		offset:  offset
		message: message
	}
}

// ---------------------------------------------------------------------------
// Numbers

const keyword_idents = ['true', 'false', 'null', 'inf', '-inf', 'nan']!

// looks_like_number reports whether an identifier-char run must be a number
// (spec: identifiers may not start with a digit, sign+digit, '.'+digit or sign+'.'+digit).
@[direct_array_access]
fn looks_like_number(s string) bool {
	if s == '' {
		return false
	}
	if s[0].is_digit() {
		return true
	}
	if (s[0] == `+` || s[0] == `-`) && s.len > 1 {
		if s[1].is_digit() {
			return true
		}
		return s[1] == `.` && s.len > 2 && s[2].is_digit()
	}
	return s[0] == `.` && s.len > 1 && s[1].is_digit()
}

// scan_digits validates `first (first | '_')*` from s[i], returns the end index or -1.
@[direct_array_access]
fn scan_digits(s string, i int, is_digit fn (u8) bool) int {
	if i >= s.len || !is_digit(s[i]) {
		return -1
	}
	mut j := i + 1
	for j < s.len && (is_digit(s[j]) || s[j] == `_`) {
		j++
	}
	return j
}

fn is_dec(b u8) bool {
	return b >= `0` && b <= `9`
}

fn is_hex(b u8) bool {
	return (b >= `0` && b <= `9`) || (b >= `a` && b <= `f`) || (b >= `A` && b <= `F`)
}

fn is_oct(b u8) bool {
	return b >= `0` && b <= `7`
}

fn is_bin(b u8) bool {
	return b == `0` || b == `1`
}

// parse_number converts a token that looks_like_number into Data, strictly
// following the `number` grammar. `s` is the raw token including its sign.
@[direct_array_access]
fn parse_number(s string) !Data {
	mut i := 0
	negative := s[0] == `-`
	if s[0] == `-` || s[0] == `+` {
		i = 1
	}
	if i + 1 < s.len && s[i] == `0` && s[i + 1] in [u8(`x`), `o`, `b`] {
		mut radix := 2
		mut end := -1
		match s[i + 1] {
			`x` {
				radix = 16
				end = scan_digits(s, i + 2, is_hex)
			}
			`o` {
				radix = 8
				end = scan_digits(s, i + 2, is_oct)
			}
			else {
				end = scan_digits(s, i + 2, is_bin)
			}
		}
		if end != s.len {
			return error('invalid number `${s}`')
		}
		return make_integer(s[i + 2..], radix, negative)
	}
	// decimal := sign? integer ('.' integer)? exponent?
	mut end := scan_digits(s, i, is_dec)
	if end < 0 {
		return error('invalid number `${s}`')
	}
	mut is_float := false
	if end < s.len && s[end] == `.` {
		is_float = true
		end = scan_digits(s, end + 1, is_dec)
		if end < 0 {
			return error('invalid number `${s}`: expected digits after `.`')
		}
	}
	if end < s.len && (s[end] == `e` || s[end] == `E`) {
		is_float = true
		mut j := end + 1
		if j < s.len && (s[j] == `+` || s[j] == `-`) {
			j++
		}
		end = scan_digits(s, j, is_dec)
		if end < 0 {
			return error('invalid number `${s}`: expected digits in exponent')
		}
	}
	if end != s.len {
		return error('invalid number `${s}`')
	}
	if is_float {
		clean := s.replace('_', '')
		f := strconv.atof64(clean) or { return error('invalid number `${s}`') }
		return Data(f)
	}
	return make_integer(s[i..], 10, negative)
}

// make_integer builds an i64 when the magnitude fits, a BigInt otherwise.
@[direct_array_access]
fn make_integer(digits string, radix int, negative bool) !Data {
	mut acc := u64(0)
	mut overflow := false
	r := u64(radix)
	for b in digits {
		if b == `_` {
			continue
		}
		d := u64(if b <= `9` {
			b - `0`
		} else if b >= `a` { b - `a` + 10 } else { b - `A` + 10 })
		if acc > (max_u64 - d) / r {
			overflow = true
			break
		}
		acc = acc * r + d
	}
	if !overflow {
		if negative {
			if acc <= u64(max_i64) + 1 {
				return Data(-i64(acc))
			}
		} else if acc <= u64(max_i64) {
			return Data(i64(acc))
		}
		return Data(BigInt{
			negative: negative
			digits:   acc.str()
		})
	}
	n := big.integer_from_radix(digits.replace('_', ''), u32(radix)) or {
		return error('invalid number `${digits}`')
	}
	return Data(BigInt{
		negative: negative
		digits:   n.str()
	})
}

// ---------------------------------------------------------------------------
// String escapes

// append_escape decodes the escape starting after the backslash at s[i]
// (s[i] is the character following `\`), appends it to sb and returns the index
// after the escape. Whitespace escapes must be handled by the caller.
@[direct_array_access]
fn append_escape(s string, i int, mut sb strings.Builder) !int {
	if i >= s.len {
		return error_at(i, 'unexpected end of input after `\\`')
	}
	match s[i] {
		`n` { sb.write_u8(`\n`) }
		`r` { sb.write_u8(`\r`) }
		`t` { sb.write_u8(`\t`) }
		`\\` { sb.write_u8(`\\`) }
		`"` { sb.write_u8(`"`) }
		`b` { sb.write_u8(0x08) }
		`f` { sb.write_u8(0x0C) }
		`s` { sb.write_u8(` `) }
		`u` {
			end := unicode_escape_end(s, i)!
			mut cp := u32(0)
			for j in i + 2 .. end - 1 {
				cp = (cp << 4) | u32(hex_val(s[j]))
			}
			if cp >= 0xD800 && cp <= 0xDFFF {
				return error_at(i - 1, 'unicode escape U+${cp:04X} is a surrogate')
			}
			if cp > 0x10FFFF {
				return error_at(i - 1, 'unicode escape U+${cp:X} is above U+10FFFF')
			}
			sb.write_rune(rune(cp))
			return end
		}
		else {
			return error_at(i - 1, 'invalid escape sequence')
		}
	}
	return i + 1
}

// unicode_escape_end checks the shape `u{` hex{1,6} `}` starting at s[i] (the
// `u`) and returns the index just after the closing brace.
@[direct_array_access]
fn unicode_escape_end(s string, i int) !int {
	if i + 1 >= s.len || s[i + 1] != `{` {
		return error_at(i - 1, 'invalid unicode escape, expected `\\u{`')
	}
	mut j := i + 2
	for j < s.len && is_hex(s[j]) {
		j++
	}
	if j >= s.len || s[j] != `}` {
		return error_at(i - 1, 'invalid unicode escape, expected `}`')
	}
	ndigits := j - (i + 2)
	if ndigits == 0 || ndigits > 6 {
		return error_at(i - 1, 'unicode escape must have 1 to 6 hex digits')
	}
	return j + 1
}

@[inline]
fn hex_val(b u8) u8 {
	if b <= `9` {
		return b - `0`
	}
	if b >= `a` {
		return b - `a` + 10
	}
	return b - `A` + 10
}

// unescape resolves every backslash escape in s. Whitespace escapes are
// resolved too, so it is only used on single-line strings and on multi-line
// strings that have already had their whitespace escapes removed.
@[direct_array_access]
fn unescape(s string) !string {
	if !s.contains('\\') {
		return s
	}
	mut sb := strings.new_builder(s.len)
	mut i := 0
	mut seg := 0
	for i < s.len {
		if s[i] != `\\` {
			i++
			continue
		}
		sb.write_string(s[seg..i])
		i = append_escape(s, i + 1, mut sb)!
		seg = i
	}
	sb.write_string(s[seg..])
	return sb.str()
}
