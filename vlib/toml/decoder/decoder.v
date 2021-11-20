// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module decoder

import toml.ast
import toml.ast.walker
import toml.token
import toml.scanner
import strconv

// Decoder decode special sequences in a tree of TOML `ast.Value`'s.
pub struct Decoder {
	scanner &scanner.Scanner
}

// decode decodes certain `ast.Value`'s and all it's children.
pub fn (d Decoder) decode(mut n ast.Value) ? {
	walker.walk_and_modify(d, mut n) ?
}

fn (d Decoder) modify(mut value ast.Value) ? {
	match value {
		ast.Quoted {
			mut v := &(value as ast.Quoted)
			d.decode_quoted(mut v) ?
		}
		else {}
	}
}

// excerpt returns a string of the token's surroundings
fn (d Decoder) excerpt(tp token.Position) string {
	return d.scanner.excerpt(tp.pos, 10)
}

// decode_quoted returns an error if `q` is not a valid quoted TOML string.
fn (d Decoder) decode_quoted(mut q ast.Quoted) ? {
	d.decode_quoted_escapes(mut q) ?
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
fn (d Decoder) decode_quoted_escapes(mut q ast.Quoted) ? {
	// Setup a scanner in stack memory for easier navigation.
	mut s := scanner.new_simple(q.text) ?

	q.text = q.text.replace('\\"', '"')

	// TODO use string builder
	mut decoded_s := ''
	// See https://toml.io/en/v1.0.0#string for more info on string types.
	is_basic := q.quote == `\"`
	if !is_basic {
		return
	}
	for {
		ch := s.next()
		if ch == scanner.end_of_text {
			break
		}
		ch_byte := byte(ch)

		if ch == `\\` {
			ch_next := byte(s.at())

			if ch_next == `\\` {
				decoded_s += ch_next.ascii_str()
				s.next()
				continue
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

			escape := ch_byte.ascii_str() + ch_next.ascii_str()
			// Decode unicode escapes
			if escape.to_lower() == '\\u' {
				// Long type Unicode (\UXXXXXXXX) is a maximum of 10 chars: '\' + 'U' + 8 hex characters
				// we pass in 10 characters from the `u`/`U` which is the longest possible sequence
				// of 9 chars plus one extra.
				mut decoded := ''
				if s.remaining() >= 10 {
					pos := s.state().pos
					decoded = d.decode_unicode_escape(s.text[pos..pos + 11]) or {
						st := s.state()
						return error(@MOD + '.' + @STRUCT + '.' + @FN +
							' escaped Unicode is invalid. $err.msg.capitalize() ($st.line_nr,$st.col) in ...${d.excerpt(q.pos)}...')
					}
					decoded_s += decoded
					s.skip_n(s.text[pos..pos + 11].len)
					continue
				} else {
					pos := s.state().pos
					decoded = d.decode_unicode_escape(s.text[pos..]) or {
						st := s.state()
						return error(@MOD + '.' + @STRUCT + '.' + @FN +
							' escaped Unicode is invalid. $err.msg.capitalize() ($st.line_nr,$st.col) in ...${d.excerpt(q.pos)}...')
					}
					decoded_s += decoded
					s.skip_n(s.text[pos..].len)
					continue
				}
			}
		}
		decoded_s += ch_byte.ascii_str()
	}
	q.text = decoded_s
}

// decode_unicode_escape returns an error if `esc_unicode` is not
// a valid Unicode escape sequence. `esc_unicode` is expected to be
// prefixed with either `u` or `U`.
fn (d Decoder) decode_unicode_escape(esc_unicode string) ?string {
	is_long_esc_type := esc_unicode.starts_with('U')
	mut sequence := esc_unicode[1..]
	hex_digits_len := if is_long_esc_type { 8 } else { 4 }

	sequence = sequence[..hex_digits_len]

	mut unicode_point := sequence
	if unicode_point.len < 8 {
		unicode_point = '0'.repeat(8 - unicode_point.len) + unicode_point
	}
	rn := rune(strconv.parse_int(unicode_point, 16, 0) ?)
	return '$rn'
}
