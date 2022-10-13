// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module decoder

import toml.ast
import toml.ast.walker
import toml.token
import toml.scanner
import strconv

const (
	// utf8_max is the largest inclusive value of the Unicodes scalar value ranges.
	utf8_max = 0x10FFFF
)

// Decoder decode special sequences in a tree of TOML `ast.Value`'s.
pub struct Decoder {
	scanner &scanner.Scanner = unsafe { nil }
}

// decode decodes certain `ast.Value`'s and all it's children.
pub fn (d Decoder) decode(mut n ast.Value) ? {
	walker.walk_and_modify(d, mut n)?
}

fn (d Decoder) modify(mut value ast.Value) ? {
	match value {
		ast.Quoted {
			mut v := &(value as ast.Quoted)
			d.decode_quoted(mut v)?
		}
		ast.Number {
			mut v := &(value as ast.Number)
			d.decode_number(mut v)?
		}
		ast.DateTime {
			mut v := &(value as ast.DateTime)
			d.decode_date_time(mut v)?
		}
		else {}
	}
}

// excerpt returns a string of the token's surroundings
fn (d Decoder) excerpt(tp token.Pos) string {
	return d.scanner.excerpt(tp.pos, 10)
}

// decode_quoted returns an error if `q` is not a valid quoted TOML string.
fn (d Decoder) decode_quoted(mut q ast.Quoted) ? {
	decode_quoted_escapes(mut q)?
}

// decode_number decodes the `n ast.Number` into valid TOML.
fn (d Decoder) decode_number(mut n ast.Number) ? {
	if n.text == '-nan' || n.text == '+nan' {
		n.text = 'nan'
	}
}

// decode_quoted_escapes returns an error for any disallowed escape sequences.
// Delimiters in TOML has significant meaning:
// '/''' delimits *literal* strings (WYSIWYG / What-you-see-is-what-you-get)
// "/""" delimits *basic* strings
// Allowed escapes in *basic* strings are:
// \b         - backspace       (U+0008)
// \t         - tab             (U+0009)
// \n         - linefeed        (U+000A)
// \f         - form feed       (U+000C)
// \r         - carriage return (U+000D)
// \"         - quote           (U+0022)
// \\         - backslash       (U+005C)
// \uXXXX     - Unicode         (U+XXXX)
// \UXXXXXXXX - Unicode         (U+XXXXXXXX)
pub fn decode_quoted_escapes(mut q ast.Quoted) ? {
	// Setup a scanner in stack memory for easier navigation.
	mut eat_whitespace := false
	// TODO use string builder
	mut decoded_s := ''
	// See https://toml.io/en/v1.0.0#string for more info on string types.
	is_basic := q.quote == `\"`
	if !is_basic {
		return
	}

	mut s := scanner.new_simple_text(q.text)?
	q.text = q.text.replace('\\"', '"')

	for {
		ch := s.next()
		if ch == scanner.end_of_text {
			break
		}
		ch_byte := u8(ch)

		if eat_whitespace && ch_byte.is_space() {
			continue
		}
		eat_whitespace = false

		if ch == `\\` {
			ch_next := s.at()
			ch_next_byte := u8(ch_next)

			if ch_next == `\\` {
				decoded_s += ch_next_byte.ascii_str()
				s.next()
				continue
			}

			if q.is_multiline {
				if ch_next_byte.is_space() {
					eat_whitespace = true
					continue
				}
			}

			if ch_next == `"` {
				decoded_s += '"'
				s.next()
				continue
			}

			if ch_next == `n` {
				decoded_s += '\n'
				s.next()
				continue
			}

			if ch_next == `t` {
				decoded_s += '\t'
				s.next()
				continue
			}

			if ch_next == `b` {
				decoded_s += '\b'
				s.next()
				continue
			}

			if ch_next == `r` {
				decoded_s += '\r'
				s.next()
				continue
			}

			if ch_next == `f` {
				decoded_s += '\f'
				s.next()
				continue
			}

			escape := ch_byte.ascii_str() + ch_next_byte.ascii_str()
			// Decode unicode escapes
			if escape.to_lower() == '\\u' {
				is_valid_short := u8(s.peek(1)).is_hex_digit() && u8(s.peek(2)).is_hex_digit()
					&& u8(s.peek(3)).is_hex_digit() && u8(s.peek(4)).is_hex_digit()

				if is_valid_short {
					is_valid_long := u8(s.peek(5)).is_hex_digit() && u8(s.peek(6)).is_hex_digit()
						&& u8(s.peek(7)).is_hex_digit() && u8(s.peek(8)).is_hex_digit()
					// If it's a long type Unicode (\UXXXXXXXX) with a maximum of 10 chars: '\' + 'U' + 8 hex characters
					// we pass in 10 characters from the `u`/`U` which is the longest possible sequence
					// of 9 chars plus one extra.
					// Else it's a short sequence (\uXXXX) with a maximum of 6 chars: '\' + 'U' + 4 hex characters.
					mut decoded := ''
					mut sequence_length := 0
					mut unicode_val := 0
					mut slen := if is_valid_long { 10 } else { 6 }
					if slen <= s.remaining() {
						pos := s.state().pos
						sequence := s.text[pos..pos + slen + 1]

						decoded, unicode_val, sequence_length = decode_unicode_escape(sequence) or {
							decoded_s += escape
							continue
						}
						if unicode_val > decoder.utf8_max || unicode_val < 0 {
							decoded_s += escape
							continue
						}
						// Check if the Unicode value is actually in the valid Unicode scalar value ranges.
						if !((unicode_val >= 0x0000 && unicode_val <= 0xD7FF)
							|| (unicode_val >= 0xE000 && unicode_val <= decoder.utf8_max)) {
							decoded_s += escape
							continue
						}
						decoded_s += decoded
						replacement := s.text[pos..pos + sequence_length + 1]
						s.skip_n(replacement.len)
						continue
					} else {
						pos := s.state().pos
						sequence := s.text[pos..]
						decoded, _, _ = decode_unicode_escape(sequence) or {
							decoded_s += escape
							continue
						}
						decoded_s += decoded
						s.skip_n(s.text[pos..].len)
						continue
					}
				}
			}
		}
		decoded_s += ch_byte.ascii_str()
	}
	q.text = decoded_s
}

// decode_unicode_escape decodes the Unicode escape sequence `esc_unicode`.
// The sequence is expected to be prefixed with either `u` or `U`.
// decode_unicode_escape returns the decoded rune as
// a string, it's integer value and it's length.
fn decode_unicode_escape(esc_unicode string) ?(string, int, int) {
	is_long_esc_type := esc_unicode.starts_with('U')
	mut sequence := esc_unicode[1..]
	hex_digits_len := if is_long_esc_type { 8 } else { 4 }
	mut sequence_len := hex_digits_len

	sequence = sequence[..hex_digits_len]

	mut unicode_point := sequence
	if unicode_point.len < 8 {
		unicode_point = '0'.repeat(8 - unicode_point.len) + unicode_point
	}
	i64_val := strconv.parse_int(unicode_point, 16, 0)?
	rn := rune(i64_val)
	return '$rn', int(i64_val), sequence_len
}

// decode_date_time decodes the `dt ast.DateTime`.
fn (d Decoder) decode_date_time(mut dt ast.DateTime) ? {
	// Expand milliseconds that are only 1 char
	if dt.text.contains('.') {
		yymmddhhmmss := dt.text.all_before('.')
		rest := dt.text.all_after('.')
		z := if rest.contains('Z') { 'Z' } else { '' }
		mut ms := rest
		mut offset := ''
		if rest.contains('+') {
			offset = '+' + rest.all_after('+')
			ms = rest.all_before('+')
		} else if rest.contains('-') {
			offset = '-' + rest.all_after('-')
			ms = rest.all_before('-')
		}
		if z != '' {
			ms = ms.replace('Z', '')
		}
		if ms.len > 1 {
			return
		}
		ms = ms + '0'.repeat(4 - ms.len) + z
		dt.text = yymmddhhmmss + '.' + ms + offset
	}
}
