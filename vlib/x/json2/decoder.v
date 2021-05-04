// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

// `Any` is a sum type that lists the possible types to be decoded and used.
pub type Any = Null | []Any | bool | f32 | f64 | i64 | int | map[string]Any | string |
	u64

// `Null` struct is a simple representation of the `null` value in JSON.
pub struct Null {
	is_null bool = true
}

struct Parser {
mut:
	scanner      &Scanner
	p_tok        Token
	tok          Token
	n_tok        Token
	n_level      int
	convert_type bool = true
}

struct InvalidTokenError {
	msg  string
	code int
}

struct UnknownTokenError {
	msg  string
	code int
}

fn (mut p Parser) next() {
	p.p_tok = p.tok
	p.tok = p.n_tok
	p.n_tok = p.scanner.scan()
}

fn (mut p Parser) next_with_err() ? {
	p.next()
	if p.tok.kind == .error {
		return error(p.emit_error(p.tok.lit.bytestr()))
	}
}

fn (p Parser) emit_error(msg string) string {
	line := p.tok.line
	column := p.tok.col + p.tok.lit.len
	return '[x.json2] $msg ($line:$column)'
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
	p.next_with_err() ?
	fi := p.decode_value() ?
	if p.tok.kind != .eof {
		return IError(&InvalidTokenError{
			msg: p.emit_error('invalid token `$p.tok.kind`')
		})
	}
	return fi
}

fn (mut p Parser) decode_value() ?Any {
	if p.n_level == 500 {
		return error(p.emit_error('reached maximum nesting level of 500'))
	}
	if (p.tok.kind == .lsbr && p.n_tok.kind == .lcbr)
		|| (p.p_tok.kind == p.tok.kind && p.tok.kind == .lsbr) {
		p.n_level++
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
			p.next_with_err() ?
			if p.convert_type {
				if kind == .float {
					return Any(tl.f64())
				}
				return Any(tl.i64())
			}
			return Any(tl)
		}
		.bool_ {
			lit := p.tok.lit.bytestr()
			p.next_with_err() ?
			if p.convert_type {
				return Any(lit.bool())
			}
			return Any(lit)
		}
		.null {
			p.next_with_err() ?
			if p.convert_type {
				return Any(null)
			}
			return Any('null')
		}
		.str_ {
			str := p.tok.lit.bytestr()
			p.next_with_err() ?
			return Any(str)
		}
		else {
			return IError(&InvalidTokenError{
				msg: p.emit_error('invalid token `$p.tok.kind`')
			})
		}
	}
	return Any(null)
}

fn (mut p Parser) decode_array() ?Any {
	mut items := []Any{}
	p.next_with_err() ?
	for p.tok.kind != .rsbr {
		item := p.decode_value() ?
		items << item
		if p.tok.kind == .comma {
			p.next_with_err() ?
			if p.tok.kind == .rsbr || p.tok.kind == .rcbr {
				return IError(&InvalidTokenError{
					msg: p.emit_error('invalid token `$p.tok.lit')
				})
			}
		} else if p.tok.kind == .rsbr {
			break
		} else {
			return IError(&UnknownTokenError{
				msg: p.emit_error("unknown token '$p.tok.lit' when decoding array.")
			})
		}
	}
	p.next_with_err() ?
	return Any(items)
}

fn (mut p Parser) decode_object() ?Any {
	mut fields := map[string]Any{}
	p.next_with_err() ?
	for p.tok.kind != .rcbr {
		is_key := p.tok.kind == .str_ && p.n_tok.kind == .colon
		if !is_key {
			return IError(&InvalidTokenError{
				msg: p.emit_error('invalid token `$p.tok.kind`, expecting `str_`')
			})
		}
		cur_key := p.tok.lit.bytestr()
		p.next_with_err() ?
		p.next_with_err() ?
		fields[cur_key] = p.decode_value() ?
		if p.tok.kind == .comma {
			p.next_with_err() ?
			if p.tok.kind != .str_ {
				return IError(&UnknownTokenError{
					msg: p.emit_error("unknown token '$p.tok.lit' when decoding object.")
				})
			}
		}
	}
	p.next_with_err() ?
	return Any(fields)
}
