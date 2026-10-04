// Copyright (c) 2026 The V Language. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module jsonc

// Span is a byte range in the source text.
struct Span {
	start int
	end   int
}

// LexResult is what one pass over the text produced: the first JSONC
// strictness violation found, and the byte range of every comment.
struct LexResult {
mut:
	violation &ParseError
	comments  []Span
}

// Cursor walks the source text as runes while keeping the three coordinates an
// error needs in step: the line, the rune column, and the byte offset.
//
// The column counts runes and the byte offset counts UTF-8 bytes, and a leading
// byte order mark advances only the offset. That matches the underlying JSON5
// scanner, so a violation reported here and a syntax error reported there name
// the same place.
//
// The offset is advanced by the width each character has in `src`, rather than
// by the width it would have when re-encoded, so a byte that is not valid UTF-8
// moves it by exactly one byte and the offset never drifts from the source.
struct Cursor {
	src  string
	text []rune
mut:
	i      int
	line   int = 1
	col    int = 1
	offset int
}

// new_cursor returns a Cursor over `text`, positioned on its first character.
fn new_cursor(text string) Cursor {
	mut c := Cursor{
		src:  text
		text: text.runes()
	}
	if c.text.len > 0 && int(c.text[0]) == r_bom {
		c.offset = c.width()
		c.i = 1
	}
	return c
}

// at returns the code point the cursor is on, or -1 at the end of the input.
fn (c &Cursor) at() int {
	if c.i < 0 || c.i >= c.text.len {
		return -1
	}
	return int(c.text[c.i])
}

// ahead returns the code point `n` positions after the cursor, or -1 when that
// is past the end of the input.
fn (c &Cursor) ahead(n int) int {
	j := c.i + n
	if j < 0 || j >= c.text.len {
		return -1
	}
	return int(c.text[j])
}

// done reports whether the cursor has consumed the whole input.
fn (c &Cursor) done() bool {
	return c.i >= c.text.len
}

// width returns the number of source bytes the character the cursor is on
// occupies, or 0 at the end of the input.
fn (c &Cursor) width() int {
	if c.i < 0 || c.i >= c.text.len {
		return 0
	}
	return source_width(c.src, c.offset, c.text[c.i])
}

// end returns the byte offset just past the character the cursor is on, which is
// where a range covering only that character ends.
fn (c &Cursor) end() int {
	return c.offset + c.width()
}

// next consumes the current character and returns its code point, or -1 at the
// end of the input. A line feed restarts the column.
fn (mut c Cursor) next() int {
	if c.i >= c.text.len {
		return -1
	}
	code := int(c.text[c.i])
	c.offset += c.width()
	c.i++
	if code == 0x0A {
		c.line++
		c.col = 1
	} else {
		c.col++
	}
	return code
}

// pos returns the location of the character the cursor is on, as an empty range
// that `record` extends to the end of the offending text.
fn (c &Cursor) pos() Pos {
	return Pos{
		line:       c.line
		col:        c.col
		offset:     c.offset
		end_offset: c.offset
	}
}

// record stores `message` as the first violation, keeping the earliest one. The
// violation covers the text from `start` up to the byte offset `end`.
fn (mut r LexResult) record(message string, start Pos, end int) {
	if r.violation != nil {
		return
	}
	r.violation = violation_at(message, Pos{
		...start
		end_offset: end
	})
}

// lex walks `text` once and returns both the strictness violations that concern
// string literals and the byte range of every comment.
//
// The walk is deliberately tolerant: an unterminated string or block comment
// stops it rather than raising an error, because the underlying JSON5 parser
// reports malformed input with better positions than this pass could.
fn lex(text string) LexResult {
	// A reference field has to be initialized explicitly, and `unsafe { nil }`
	// is the idiom for "no violation yet".
	mut res := LexResult{
		violation: unsafe { nil }
	}
	mut c := new_cursor(text)
	for !c.done() {
		code := c.at()
		if code == r_slash {
			next := c.ahead(1)
			if next == r_slash {
				lex_line_comment(mut c, mut res)
				continue
			}
			if next == r_star {
				lex_block_comment(mut c, mut res)
				continue
			}
		}
		if code == r_quote_dbl || code == r_quote_sgl {
			lex_string(mut c, code, mut res)
			continue
		}
		// The token pass never sees whitespace, because the shared scanner skips
		// it as trivia, so the raw walk is the only place the dialect's narrower
		// set can be enforced.
		if is_json5_whitespace(code) {
			res.record('U+${code:04X} is not whitespace between JSON tokens', c.pos(),
				c.end())
		}
		c.next()
	}
	return res
}

// lex_line_comment consumes a `//` comment, up to but not including the line
// terminator that ends it.
fn lex_line_comment(mut c Cursor, mut res LexResult) {
	start := c.offset
	for !c.done() && !is_line_terminator(c.at()) {
		c.next()
	}
	res.comments << Span{
		start: start
		end:   c.offset
	}
}

// lex_block_comment consumes a `/* */` comment. An unterminated one runs to the
// end of the input.
fn lex_block_comment(mut c Cursor, mut res LexResult) {
	start := c.offset
	c.next() // `/`
	c.next() // `*`
	for !c.done() {
		if c.at() == r_star && c.ahead(1) == r_slash {
			c.next()
			c.next()
			break
		}
		c.next()
	}
	res.comments << Span{
		start: start
		end:   c.offset
	}
}

// is_rfc_escape reports whether `code` is one of the seven escapes RFC 8259
// allows without a numeric argument.
fn is_rfc_escape(code int) bool {
	return code == r_quote_dbl || code == r_backslash || code == r_slash || code == r_lower_b
		|| code == r_lower_f || code == r_lower_n || code == r_lower_r || code == r_lower_t
}

// lex_string consumes one string literal and reports the ways in which it can
// fall outside JSONC: a single-quoted delimiter, an escape sequence outside the
// RFC 8259 set, and an unescaped control character.
//
// An unterminated literal stops the scan and is left to the JSON5 parser.
fn lex_string(mut c Cursor, quote_code int, mut res LexResult) {
	if quote_code == r_quote_dbl {
		lex_string_body(mut c, quote_code, mut res)
		return
	}
	// The opening quote of a single-quoted literal comes before anything inside
	// it, so it is the violation to report, and the body is walked only to find
	// where the literal ends: the reported range covers all of it.
	start := c.pos()
	mut body := LexResult{
		violation: unsafe { nil }
	}
	lex_string_body(mut c, quote_code, mut body)
	res.record('a single-quoted string is not valid JSONC', start, c.offset)
}

// lex_string_body consumes a string literal delimited by `quote_code`, the
// cursor being on its opening quote, and reports what inside it is not JSONC.
fn lex_string_body(mut c Cursor, quote_code int, mut res LexResult) {
	c.next() // opening quote
	for !c.done() {
		code := c.at()
		if code == quote_code {
			c.next()
			return
		}
		if code == r_backslash {
			start := c.pos()
			c.next()
			if c.done() {
				return
			}
			lex_escape(mut c, start, mut res)
			continue
		}
		if code <= r_max_control {
			res.record('a control character must be escaped inside a string', c.pos(),
				c.end())
			c.next()
			continue
		}
		c.next()
	}
}

// lex_escape consumes the body of an escape sequence, the backslash at `start`
// having already been consumed, and reports it when it is not valid JSONC.
//
// The reported range starts at the backslash and ends after the escaped
// character, or, for a malformed `\u`, after the hexadecimal digits that do
// follow it. The rest of a malformed `\u` is left to the string walk, one
// character at a time, so the scan stays in step with the text even though the
// literal is already invalid.
fn lex_escape(mut c Cursor, start Pos, mut res LexResult) {
	code := c.at()
	if is_line_terminator(code) {
		if code == 0x0D && c.ahead(1) == 0x0A {
			c.next()
		}
		c.next()
		res.record('an escaped line break is not valid JSONC', start, c.offset)
		return
	}
	if code == r_lower_u {
		// The cursor is on the `u` itself, which is not one of the four digits
		// that must follow it.
		c.next()
		for _ in 0 .. 4 {
			if !is_hex_digit(c.at()) {
				res.record('`\\u` must be followed by four hexadecimal digits', start,
					c.offset)
				return
			}
			c.next()
		}
		return
	}
	if !is_rfc_escape(code) {
		res.record('`\\${rune(code).str()}` is not a valid JSONC escape sequence', start,
			c.end())
	}
	c.next()
}

// strip_comments returns `text` with every comment replaced by spaces.
//
// The replacement is byte for byte, and a line terminator inside a comment is
// kept, so every remaining character keeps the offset it had in the input. That
// lets a caller report a position in the stripped text against the original
// file. A comment that is never closed is stripped to the end of the input.
pub fn strip_comments(text string) string {
	res := lex(text)
	if res.comments.len == 0 {
		return text
	}
	mut out := text.bytes()
	for c in res.comments {
		for i in c.start .. c.end {
			if out[i] != 0x0A && out[i] != 0x0D {
				out[i] = ` `
			}
		}
	}
	return out.bytestr()
}
