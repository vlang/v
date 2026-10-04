// Copyright (c) 2026 The V Language. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module jsonc

import encoding.utf8

// ASCII code points the lexical pass compares against. They are compared as
// `int`, because V promotes a `rune` comparison to a string comparison, which
// is both slower and wrong for code points above 0x7F.
const r_star = 0x2A
const r_quote_dbl = 0x22
const r_quote_sgl = 0x27
const r_slash = 0x2F
const r_zero = 0x30
const r_nine = 0x39
const r_upper_a = 0x41
const r_upper_f = 0x46
const r_backslash = 0x5C
const r_lower_a = 0x61
const r_lower_b = 0x62
const r_lower_f = 0x66
const r_lower_n = 0x6E
const r_lower_r = 0x72
const r_lower_t = 0x74
const r_lower_u = 0x75
const r_bom = 0xFEFF

// `string.runes()` decodes every byte that does not start a valid UTF-8 sequence
// as this replacement character, consuming that one byte.
const r_replacement = 0xFFFD

// RFC 8259 restricts unescaped characters inside a string to everything above
// U+001F; the remaining codes are the control characters it forbids.
const r_max_control = 0x1F

// The characters the shared JSON5 scanner treats as whitespace but RFC 8259 does
// not allow between tokens. Its `is_whitespace` accepts all of these, so they
// are removed before the token pass can see them.
const r_vt = 0x0B
const r_ff = 0x0C
const r_nbsp = 0xA0

// Pos is a location in the source text. Unlike the position carried by a
// syntax error from the underlying JSON5 parser, it also knows the byte range
// the offending text occupies, so an editor can underline exactly it.
pub struct Pos {
pub:
	line       int
	col        int
	offset     int // byte offset of the first character
	end_offset int // byte offset one past the last character
}

// ParseError describes input that is well formed JSON5 but not valid JSONC.
//
// JSONC is RFC 8259 plus `//` and `/* */` comments, so a ParseError always
// names a JSON5 extension that strict mode rejects. Malformed JSON is not
// reported this way: it is reported by the underlying JSON5 parser, whose
// error is returned unchanged.
pub struct ParseError {
	Error
pub:
	message string
	pos     Pos
}

// msg formats a ParseError for `IError.msg()`.
pub fn (e &ParseError) msg() string {
	return 'jsonc: ${e.pos.line}:${e.pos.col}: ${e.message}'
}

// violation_at builds a ParseError for `message` at `pos`. Both passes fill in
// the end of the range themselves, from the width of the text they report.
fn violation_at(message string, pos Pos) &ParseError {
	return &ParseError{
		message: message
		pos:     pos
	}
}

// rune_bytes returns the number of bytes `r` occupies when encoded as UTF-8.
fn rune_bytes(r rune) int {
	code := int(r)
	if code < 0x80 {
		return 1
	}
	if code < 0x800 {
		return 2
	}
	if code < 0x10000 {
		return 3
	}
	return 4
}

// source_width returns the number of bytes of `src`, starting at `offset`, that
// `r` was decoded from by `string.runes()`.
//
// That is the UTF-8 width of `r`, except for a replacement character standing in
// for a byte that is not valid UTF-8: the decoder consumes only that byte. A
// replacement character is three bytes wide only when the source spells it out.
fn source_width(src string, offset int, r rune) int {
	if int(r) == r_replacement && !(offset + 2 < src.len && src[offset] == 0xEF
		&& src[offset + 1] == 0xBF && src[offset + 2] == 0xBD) {
		return 1
	}
	return rune_bytes(r)
}

// is_digit_byte reports whether `b` is an ASCII decimal digit, for the single
// bytes a number literal is walked through.
fn is_digit_byte(b u8) bool {
	return b >= r_zero && b <= r_nine
}

// is_nonzero_digit_byte reports whether `b` is an ASCII decimal digit other than
// `0`, which is what the first digit of a multi-digit integer must be.
fn is_nonzero_digit_byte(b u8) bool {
	return b > r_zero && b <= r_nine
}

// is_rfc_whitespace reports whether `code` is one of the four characters RFC 8259
// allows between tokens.
fn is_rfc_whitespace(code int) bool {
	return code == 0x20 || code == 0x09 || code == 0x0A || code == 0x0D
}

// is_json5_whitespace reports whether `code` is whitespace that the shared JSON5
// scanner skips but that RFC 8259 does not allow between tokens.
//
// A leading byte order mark is not reported: `new_cursor` steps over it before
// the walk starts, so only an interior one reaches this test.
fn is_json5_whitespace(code int) bool {
	if is_rfc_whitespace(code) {
		return false
	}
	return code == r_vt || code == r_ff || code == r_nbsp || code == r_bom
		|| (code > 0x2000 && utf8.is_space(rune(code)))
}

// is_hex_digit reports whether `code` is an ASCII hexadecimal digit.
fn is_hex_digit(code int) bool {
	return (code >= r_zero && code <= r_nine) || (code >= r_lower_a && code <= r_lower_f)
		|| (code >= r_upper_a && code <= r_upper_f)
}

// is_line_terminator reports whether `code` ends a line for the purposes of a
// `//` comment.
fn is_line_terminator(code int) bool {
	return code == 0x0A || code == 0x0D || code == 0x2028 || code == 0x2029
}
