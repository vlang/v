// Copyright (c) 2026 The V Language. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module jsonc

import x.json5

// Container names the kind of collection a token sits directly inside, which is
// what tells an unquoted object key apart from a bare value.
enum Container {
	object
	array
}

// number_violation returns the reason `lit` is not a valid JSON number, or an
// empty string when it is one.
//
// The two JSON5-only spellings that are worth naming are handled first, because
// `0x10` and `+1` would otherwise be reported as generic grammar failures. A `-`
// is legal in JSON, so it is peeled off here and the rest is checked against the
// RFC 8259 grammar.
fn number_violation(lit string) string {
	if lit.len == 0 {
		return ''
	}
	if lit[0] == `+` {
		return '`${lit}` has a leading `+`, which JSON does not allow'
	}
	digits := lit.trim_left('+-')
	if digits.len == 0 {
		return ''
	}
	if digits.len > 1 && digits[0] == `0` && (digits[1] == `x` || digits[1] == `X`) {
		return '`${lit}` is a hexadecimal literal, which JSON does not allow'
	}
	return grammar_violation(lit, digits)
}

// grammar_violation checks `digits`, the number literal `lit` without its sign,
// against the RFC 8259 grammar:
//
//	number = [ minus ] int [ frac ] [ exp ]
//	int    = zero / ( digit1-9 *DIGIT )
//	frac   = decimal-point 1*DIGIT
//	exp    = e [ minus / plus ] 1*DIGIT
//
// Every part is optional except the integer, and each one that is present must
// be followed by at least one digit, so `1.`, `1.e2` and `1e` are all refused.
// The integer may not carry a leading zero unless it is a bare `0`, which rules
// out `01` and `00.5`.
fn grammar_violation(lit string, digits string) string {
	mut i := 0
	if digits[i] == `0` {
		i++
		if i < digits.len && is_digit_byte(digits[i]) {
			return '`${lit}` has a leading zero, which JSON does not allow'
		}
	} else if digits[i] == `.` {
		return '`${lit}` has a leading dot, which JSON does not allow'
	} else if !is_nonzero_digit_byte(digits[i]) {
		return '`${lit}` is not a valid JSON number'
	}
	for i < digits.len && is_digit_byte(digits[i]) {
		i++
	}
	if i < digits.len && digits[i] == `.` {
		i++
		start := i
		for i < digits.len && is_digit_byte(digits[i]) {
			i++
		}
		if i == start {
			return '`${lit}` has no digit after the decimal point'
		}
	}
	if i < digits.len && (digits[i] == `e` || digits[i] == `E`) {
		i++
		if i < digits.len && (digits[i] == `+` || digits[i] == `-`) {
			i++
		}
		start := i
		for i < digits.len && is_digit_byte(digits[i]) {
			i++
		}
		if i == start {
			return '`${lit}` has no digit in the exponent'
		}
	}
	if i != digits.len {
		return '`${lit}` is not a valid JSON number'
	}
	return ''
}

// check_token returns the first JSON5 extension in `cur` that is not valid JSONC,
// or an empty string when the token is acceptable.
//
// `prev` is the kind of the previous significant token and `next` is the token
// after `cur`, which together with `inside` are what a rule needs: a token is an
// object key only directly after `{` or `,`, and a comma ends a collection only
// when a closing bracket follows it.
fn check_token(cur json5.Token, prev json5.TokenKind, next json5.Token, inside Container, opts ParseOpts) string {
	if cur.kind == .comma && (next.kind == .rcbr || next.kind == .rsbr) {
		if !opts.allow_trailing_comma {
			return 'a trailing comma is not valid JSONC'
		}
		return ''
	}
	// Key position is tested before the value rules, so that `{Infinity: 1}` is
	// reported as the unquoted key it is rather than as an invalid value.
	if inside == .object && (prev == .lcbr || prev == .comma) {
		match cur.kind {
			.ident, .number, .bool, .null, .infinity, .nan {
				return 'the key `${cur.lit}` is not quoted, which JSON does not allow'
			}
			else {}
		}
		return ''
	}
	if cur.kind == .infinity || cur.kind == .nan {
		return '`${cur.lit}` is not a valid JSON value'
	}
	if cur.kind == .number {
		return number_violation(cur.lit)
	}
	return ''
}

// validate walks the token stream of `text` and returns the first JSON5
// extension that JSONC does not allow, or nil when the document is valid.
//
// It relies on the JSON5 scanner reporting a token's line and column, so the
// byte offsets it fills in are recovered from `text` with the same line and
// column counting.
//
// Callers run the JSON5 parser first, so the text is well formed and the scanner
// cannot fail; the error arm below is unreachable in practice and reports no
// violation rather than inventing one.
fn validate(text string, opts ParseOpts) &ParseError {
	scanner := json5.new_scanner(text)
	mut stack := []Container{}
	mut prev := json5.TokenKind.none
	mut cur := scanner.next() or { return nil }
	for cur.kind != .eof {
		next := scanner.next() or { return nil }
		inside := if stack.len > 0 { stack[stack.len - 1] } else { Container.array }
		message := check_token(cur, prev, next, inside, opts)
		if message != '' {
			return violation_at(message, locate(text, cur))
		}
		match cur.kind {
			.lcbr {
				stack << Container.object
			}
			.lsbr {
				stack << Container.array
			}
			.rcbr, .rsbr {
				if stack.len > 0 {
					stack.delete_last()
				}
			}
			else {}
		}
		prev = cur.kind
		cur = next
	}
	return nil
}

// locate converts the line and column of a JSON5 token into a Pos, recovering
// the byte offsets from `text`.
//
// The extent is the token's own source text. Every token this module reports on
// carries its raw spelling in `lit` — an unquoted key, a number, a keyword, or
// the comma itself — so its byte length is the width to underline. A token whose
// `lit` has been decoded, such as a string, is never reported here, because the
// rune pass owns string literals and measures them as it walks.
//
// The walk uses the rune pass's Cursor, which counts lines and columns the way
// the JSON5 scanner does, including stepping over a leading byte order mark
// without counting a column for it, and which keeps the byte offset in step
// with the source even across bytes that are not valid UTF-8.
fn locate(text string, tok json5.Token) Pos {
	mut c := new_cursor(text)
	for !c.done() && (c.line != tok.pos.line || c.col != tok.pos.col) {
		c.next()
	}
	return Pos{
		line:       tok.pos.line
		col:        tok.pos.col
		offset:     c.offset
		end_offset: c.offset + tok.lit.len
	}
}
