// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

fn format_message(msg string, line int, column int) string {
	return '[x.json2] ${msg} (${line}:${column})'
}

pub struct DecodeError {
	line    int
	column  int
	message string
}

// code returns the error code of DecodeError
pub fn (err DecodeError) code() int {
	return 3
}

// msg returns the message of the DecodeError
pub fn (err DecodeError) msg() string {
	return format_message(err.message, err.line, err.column)
}

pub struct InvalidTokenError {
	DecodeError
	token    Token
	expected TokenKind
}

// code returns the error code of the InvalidTokenError
pub fn (err InvalidTokenError) code() int {
	return 2
}

// msg returns the message of the InvalidTokenError
pub fn (err InvalidTokenError) msg() string {
	footer_text := if err.expected != .none_ { ', expecting `${err.expected}`' } else { '' }
	return format_message('invalid token `${err.token.kind}`${footer_text}', err.token.line,
		err.token.full_col())
}

pub struct UnknownTokenError {
	DecodeError
	token Token
	kind  ValueKind = .unknown
}

// code returns the error code of the UnknownTokenError
pub fn (err UnknownTokenError) code() int {
	return 1
}

// msg returns the error message of the UnknownTokenError
pub fn (err UnknownTokenError) msg() string {
	return format_message("unknown token '${err.token.lit}' when decoding ${err.kind}.",
		err.token.line, err.token.full_col())
}

struct Parser {
pub mut:
	scanner &Scanner = unsafe { nil }
	// previous token
	previous_tok Token
	tok          Token
	// next token
	next_tok     Token
	n_level      int
	convert_type bool = true
}

fn (mut p Parser) next() {
	p.previous_tok = p.tok
	p.tok = p.next_tok
	p.next_tok = p.scanner.scan()
}

fn (mut p Parser) next_with_err() ! {
	p.next()
	if p.tok.kind == .error {
		return DecodeError{
			line: p.tok.line
			column: p.tok.full_col()
			message: p.tok.lit.bytestr()
		}
	}
}

// TODO: copied from v.util to avoid the entire module and its functions
// from being imported. remove later once -skip-unused is enabled by default.
// skip_bom - skip Byte Order Mark (BOM)
// The UTF-8 BOM is a sequence of Bytes at the start of a text-stream (EF BB BF or \ufeff)
// that allows the reader to reliably determine if file is being encoded in UTF-8.
fn skip_bom(file_content string) string {
	mut raw_text := file_content
	// BOM check
	if raw_text.len >= 3 {
		unsafe {
			c_text := raw_text.str
			if c_text[0] == 0xEF && c_text[1] == 0xBB && c_text[2] == 0xBF {
				// skip three BOM bytes
				offset_from_begin := 3
				raw_text = tos(c_text[offset_from_begin], vstrlen(c_text) - offset_from_begin)
			}
		}
	}
	return raw_text
}

// new_parser - create a instance of Parser{}
fn new_parser(srce string, convert_type bool) Parser {
	src := skip_bom(srce)
	return Parser{
		scanner: &Scanner{
			text: src.bytes()
		}
		convert_type: convert_type
	}
}

// decode - decodes provided JSON
pub fn (mut p Parser) decode() !Any {
	p.next()
	p.next_with_err()!
	fi := p.decode_value()!
	if p.tok.kind != .eof {
		return InvalidTokenError{
			token: p.tok
		}
	}
	return fi
}

fn (mut p Parser) decode_value() !Any {
	if p.n_level + 1 == 500 {
		return DecodeError{
			message: 'reached maximum nesting level of 500'
		}
	}
	match p.tok.kind {
		.lsbr {
			return p.decode_array()
		}
		.lcbr {
			return p.decode_struct()
		}
		.int_, .float {
			tl := p.tok.lit.bytestr()
			kind := p.tok.kind
			p.next_with_err()!
			if p.convert_type {
				$if !nofloat ? {
					if kind == .float {
						return Any(tl.f64())
					}
				}
				return Any(tl.i64())
			}
			return Any(tl)
		}
		.bool_ {
			lit := p.tok.lit.bytestr()
			p.next_with_err()!
			if p.convert_type {
				return Any(lit.bool())
			}
			return Any(lit)
		}
		.null {
			p.next_with_err()!
			if p.convert_type {
				return Any(null)
			}
			return Any('null')
		}
		.str_ {
			str := p.tok.lit.bytestr()
			p.next_with_err()!
			return Any(str)
		}
		else {
			return InvalidTokenError{
				token: p.tok
			}
		}
	}
	return Any(null)
}

[manualfree]
fn (mut p Parser) decode_array() !Any {
	mut items := []Any{}
	p.next_with_err()!
	p.n_level++
	for p.tok.kind != .rsbr {
		item := p.decode_value()!
		items << item
		if p.tok.kind == .comma {
			p.next_with_err()!
			if p.tok.kind == .rsbr {
				return InvalidTokenError{
					token: p.tok
				}
			}
		} else if p.tok.kind != .rsbr {
			return UnknownTokenError{
				token: p.tok
				kind: .array
			}
		}
	}
	p.next_with_err()!
	p.n_level--
	return Any(items)
}

fn (mut p Parser) decode_struct() !Any {
	mut fields := map[string]Any{}
	p.next_with_err()!
	p.n_level++
	for p.tok.kind != .rcbr {
		if p.tok.kind != .str_ {
			return InvalidTokenError{
				token: p.tok
				expected: .str_
			}
		}

		cur_key := p.tok.lit.bytestr()
		p.next_with_err()!
		if p.tok.kind != .colon {
			return InvalidTokenError{
				token: p.tok
				expected: .colon
			}
		}

		p.next_with_err()!
		fields[cur_key] = p.decode_value()!
		if p.tok.kind != .comma && p.tok.kind != .rcbr {
			return UnknownTokenError{
				token: p.tok
				kind: .object
			}
		} else if p.tok.kind == .comma {
			p.next_with_err()!
		}
	}
	p.next_with_err()!
	p.n_level--
	return Any(fields)
}
