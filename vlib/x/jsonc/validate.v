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
// JSON5 widens the JSON number grammar in five ways: a leading `+`, a leading or
// trailing dot, and a `0x` prefix for a hexadecimal integer. A `-` is legal in
// JSON, so only a `+` is rejected here, before the sign is peeled off, because
// the remaining three forms may carry either sign.
fn number_violation(lit string) string {
	if lit.len > 0 && lit[0] == `+` {
		return '`${lit}` has a leading `+`, which JSON does not allow'
	}
	digits := lit.trim_left('+-')
	if digits.len == 0 {
		return ''
	}
	if digits.len > 1 && digits[0] == `0` && (digits[1] == `x` || digits[1] == `X`) {
		return '`${lit}` is a hexadecimal literal, which JSON does not allow'
	}
	first := digits[0]
	if first == `.` {
		return '`${lit}` has a leading dot, which JSON does not allow'
	}
	if digits[digits.len - 1] == `.` {
		return '`${lit}` has a trailing dot, which JSON does not allow'
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
			pos := json5.Token{
				kind: cur.kind
				pos:  cur.pos
			}
			return violation_at(message, locate(text, pos))
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
// A leading byte order mark is skipped because the JSON5 scanner drops it before
// it starts counting, so the first reported column is 1 even though the first
// byte of the file is at offset 0.
fn locate(text string, tok json5.Token) Pos {
	mut offset := 0
	if text.len > 0 && text[0] == 0xEF && text.len > 1 && text[1] == 0xBB {
		offset = 3
	}
	runes := text[offset..].runes()
	mut line := 1
	mut col := 1
	for r in runes {
		if line == tok.pos.line && col == tok.pos.col {
			break
		}
		offset += rune_bytes(r)
		if int(r) == 0x0A {
			line++
			col = 1
		} else {
			col++
		}
	}
	return Pos{
		line:       tok.pos.line
		col:        tok.pos.col
		offset:     offset
		end_offset: offset
	}
}
