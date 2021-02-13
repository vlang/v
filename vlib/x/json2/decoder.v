// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

import v.util

// `Any` is a sum type that lists the possible types to be decoded and used.
pub type Any = string | int | i64 | f32 | f64 | bool | Null | []Any | map[string]Any

// `Null` struct is a simple representation of the `null` value in JSON.
pub struct Null {
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

fn (mut p Parser) next() {
	p.p_tok = p.tok
	p.tok = p.n_tok
	p.n_tok = p.scanner.scan()
}

fn (mut p Parser) next_with_err() ? {
	p.next()
	if p.tok.kind == .error {
		return error(p.emit_error(p.n_tok.lit.bytestr(), p.n_tok.line, p.n_tok.col))
	}
}

fn (p Parser) emit_error(msg string, line int, column int) string {
	return '[x.json2] $msg ($line:$column)'
}

fn new_parser(srce string, convert_type bool) Parser {
	src := util.skip_bom(srce)
	return Parser{
		scanner: &Scanner{ text: src.bytes() }
		convert_type: convert_type
	}
}

fn (mut p Parser) decode() ?Any {
	mut is_valid := false
	p.next()
	p.next_with_err() ?
	if p.tok.kind in [.lcbr, .lsbr, .str_ .true_, .false_, .int_, .float, .null] {
		is_valid = true
	}
	if !is_valid {
		return error(p.emit_error('invalid JSON', 0, 0))
	}
	fi := p.decode_value() ?
	if p.tok.kind != .eof {
		return error(p.emit_error('invalid token `$p.tok.kind`', p.tok.line, p.tok.col))
	}
	return fi
}

fn (mut p Parser) decode_value() ?Any {
	if p.n_level == 500 {
		return error('reached maximum nesting level of 500.')
	}
	if (p.tok.kind == .lsbr && p.n_tok.kind == .lcbr) ||
		(p.p_tok.kind == p.tok.kind && p.tok.kind == .lsbr) {
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
			return p.decode_number()
		}
		.false_, .true_ {
			lit := p.tok.lit.bytestr()
			p.next_with_err() ?
			return if p.convert_type {
				Any(lit.bool())
			} else {
				Any(lit)
			}
		}
		.null {
			p.next_with_err() ?
			return if p.convert_type {
				Any(json2.null)
			} else {
				Any('null')
			}
		}
		.str_ {
			return p.decode_string()
		}
		else {
			return error(p.emit_error('invalid token `$p.tok.kind`', p.tok.line, p.tok.col))
		}
	}
	return Any{}
}

fn (mut p Parser) decode_string() ?Any {
	str := p.tok.lit.bytestr()
	p.next_with_err() ?
	return Any(str)
}

// now returns string instead of int or float
fn (mut p Parser) decode_number() ?Any {
	tl := p.tok.lit.bytestr()
	kind := p.tok.kind
	p.next_with_err() ?
	if p.convert_type {
		return if kind == .float {
			Any(tl.f64())
		} else {
			Any(tl.i64())
		}
	}
	return Any(tl)
}

fn (mut p Parser) decode_array() ?Any {
	mut items := []Any{}
	p.next_with_err() ?
	for p.tok.kind != .rsbr {
		item := p.decode_value() ?
		items << item
		if p.tok.kind == .comma {
			p.next_with_err() ?
			if p.tok.kind in [.rsbr, .rcbr] {
				return error(p.emit_error('invalid token `$p.tok.lit', p.tok.line, p.tok.col))
			}
		} else if p.tok.kind == .rsbr {
			break
		} else {
			return error("unknown token '$p.tok.lit' when decoding array.")
		}
	}
	p.next_with_err() ?
	return Any(items)
}

fn (mut p Parser) decode_object() ?Any {
	mut fields := map[string]Any{}
	mut cur_key := ''
	p.next_with_err() ?
	for p.tok.kind != .rcbr {
		is_key := p.tok.kind == .str_ && p.n_tok.kind == .colon
		if !is_key {
			return error("invalid token `$p.tok.kind`, expecting `string`")
		}
		cur_key = p.tok.lit.bytestr()
		p.next_with_err() ?
		p.next_with_err() ?
		fields[cur_key] = p.decode_value() ?
		if p.tok.kind == .comma {
			p.next_with_err() ?
			if p.tok.kind != .str_ {
				return error("unknown token '$p.tok.lit' when decoding object.")
			}
		}
	}
	p.next_with_err() ?
	return Any(fields)
}
