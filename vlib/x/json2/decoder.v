// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

// `Any` is a sum type that lists the possible types to be decoded and used.
pub type Any = Null | []Any | bool | f32 | f64 | i64 | int | map[string]Any | string | u64

// `Null` struct is a simple representation of the `null` value in JSON.
pub struct Null {
	is_null bool = true
}

pub enum ValueKind {
	unknown
	array
	object
	string_
	number
}

// str returns the string representation of the specific ValueKind
pub fn (k ValueKind) str() string {
	return match k {
		.unknown { 'unknown' }
		.array { 'array' }
		.object { 'object' }
		.string_ { 'string' }
		.number { 'number' }
	}
}

fn format_message(msg string, line int, column int) string {
	return '[x.json2] $msg ($line:$column)'
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
	footer_text := if err.expected != .none_ { ', expecting `$err.expected`' } else { '' }
	return format_message('invalid token `$err.token.kind`$footer_text', err.token.line,
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
	return format_message("unknown token '$err.token.lit' when decoding ${err.kind}.",
		err.token.line, err.token.full_col())
}

struct Parser {
mut:
	scanner      &Scanner = unsafe { nil }
	p_tok        Token
	tok          Token
	n_tok        Token
	n_level      int
	convert_type bool = true
}

fn (mut p Parser) next() {
	p.p_tok = p.tok
	p.tok = p.n_tok
	p.n_tok = p.scanner.scan()
}

fn (mut p Parser) next_with_err() ? {
	p.next()
	if p.tok.kind == .error {
		return IError(DecodeError{
			line: p.tok.line
			column: p.tok.full_col()
			message: p.tok.lit.bytestr()
		})
	}
}

// TODO: copied from v.util to avoid the entire module and its functions
// from being imported. remove later once -skip-unused is enabled by default.
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

fn new_parser(srce string, convert_type bool) Parser {
	src := skip_bom(srce)
	return Parser{
		scanner: &Scanner{
			text: src.bytes()
		}
		convert_type: convert_type
	}
}

fn (mut p Parser) decode() ?Any {
	p.next()
	p.next_with_err()?
	fi := p.decode_value()?
	if p.tok.kind != .eof {
		return IError(InvalidTokenError{
			token: p.tok
		})
	}
	return fi
}

fn (mut p Parser) decode_value() ?Any {
	if p.n_level + 1 == 500 {
		return IError(DecodeError{
			message: 'reached maximum nesting level of 500'
		})
	}
	match p.tok.kind {
		.lsbr {
			return p.decode_array()
		}
		.lcbr {
			return p.decode_object()
		}
		.int_, .float {
			tl := p.tok.lit.bytestr()
			kind := p.tok.kind
			p.next_with_err()?
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
			p.next_with_err()?
			if p.convert_type {
				return Any(lit.bool())
			}
			return Any(lit)
		}
		.null {
			p.next_with_err()?
			if p.convert_type {
				return Any(null)
			}
			return Any('null')
		}
		.str_ {
			str := p.tok.lit.bytestr()
			p.next_with_err()?
			return Any(str)
		}
		else {
			return IError(InvalidTokenError{
				token: p.tok
			})
		}
	}
	return Any(null)
}

[manualfree]
fn (mut p Parser) decode_array() ?Any {
	mut items := []Any{}
	p.next_with_err()?
	p.n_level++
	for p.tok.kind != .rsbr {
		item := p.decode_value()?
		items << item
		if p.tok.kind == .comma {
			p.next_with_err()?
			if p.tok.kind == .rsbr {
				return IError(InvalidTokenError{
					token: p.tok
				})
			}
		} else if p.tok.kind != .rsbr {
			return IError(UnknownTokenError{
				token: p.tok
				kind: .array
			})
		}
	}
	p.next_with_err()?
	p.n_level--
	return Any(items)
}

fn (mut p Parser) decode_object() ?Any {
	mut fields := map[string]Any{}
	p.next_with_err()?
	p.n_level++
	for p.tok.kind != .rcbr {
		if p.tok.kind != .str_ {
			return IError(InvalidTokenError{
				token: p.tok
				expected: .str_
			})
		}

		cur_key := p.tok.lit.bytestr()
		p.next_with_err()?
		if p.tok.kind != .colon {
			return IError(InvalidTokenError{
				token: p.tok
				expected: .colon
			})
		}

		p.next_with_err()?
		fields[cur_key] = p.decode_value()?
		if p.tok.kind != .comma && p.tok.kind != .rcbr {
			return IError(UnknownTokenError{
				token: p.tok
				kind: .object
			})
		} else if p.tok.kind == .comma {
			p.next_with_err()?
		}
	}
	p.next_with_err()?
	p.n_level--
	return Any(fields)
}
