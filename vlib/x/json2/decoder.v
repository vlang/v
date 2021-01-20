// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

import strings
import strconv
import v.scanner
import v.token
import v.util
import v.pref

// `Any` is a sum type that lists the possible types to be decoded and used.
pub type Any = string | int | i64 | f32 | f64 | bool | Null | []Any | map[string]Any

// `Null` struct is a simple representation of the `null` value in JSON.
pub struct Null {
}

enum ParseMode {
	array
	bool
	invalid
	null
	number
	object
	string
}

const (
	formfeed_err = 'formfeed not allowed.'
	eof_err      = 'reached eof. data not closed properly.'
)

struct Parser {
mut:
	scanner      &scanner.Scanner
	p_tok        token.Token
	tok          token.Token
	n_tok        token.Token
	mode         ParseMode = .invalid
	n_level      int
	convert_type bool = true
}

fn (mut p Parser) next() {
	p.p_tok = p.tok
	p.tok = p.n_tok
	p.n_tok = p.scanner.scan()
}

fn (p Parser) emit_error(msg string) string {
	source := p.scanner.text
	cur := p.tok
	mut pp := util.imax(0, util.imin(source.len - 1, cur.pos))
	if source.len > 0 {
		for pp >= 0 {
			if source[pp] !in [`\r`, `\n`] {
				pp--
				continue
			}
			break
		}
	}
	column := util.imax(0, cur.pos - pp + cur.len - 1)
	line := cur.line_nr
	return '[json] $msg ($line:$column)'
}

fn new_parser(srce string, convert_type bool) Parser {
	mut src := srce
	// from v/util/util.v
	if src.len >= 3 {
		c_text := src.str
		unsafe {
			if c_text[0] == 0xEF && c_text[1] == 0xBB && c_text[2] == 0xBF {
				// skip three BOM bytes
				offset_from_begin := 3
				src = tos(c_text[offset_from_begin], vstrlen(c_text) - offset_from_begin)
			}
		}
	}
	return Parser{
		scanner: scanner.new_scanner(src, .parse_comments, &pref.Preferences{})
		convert_type: convert_type
	}
}

fn check_valid_hex(str string) ? {
	if str.len != 4 {
		return error('hex string must be 4 characters.')
	}
	for l in str {
		if l.is_hex_digit() {
			continue
		}
		return error('provided string is not a hex digit.')
	}
}

fn (mut p Parser) decode() ?Any {
	p.detect_parse_mode()
	if p.mode == .invalid {
		return error(p.emit_error('invalid JSON.'))
	}
	fi := p.decode_value() or {
		return error(p.emit_error(err))
	}
	if p.tok.kind != .eof {
		return error(p.emit_error('unknown token `$p.tok.kind`.'))
	}
	return fi
}

fn (p Parser) is_formfeed() bool {
	prev_tok_pos := p.p_tok.pos + p.p_tok.len - 2
	if prev_tok_pos < p.scanner.text.len && p.scanner.text[prev_tok_pos] == 0x0c {
		return true
	}
	return false
}

fn (p Parser) is_singlequote() bool {
	src := p.scanner.text
	prev_tok_pos := p.p_tok.pos + p.p_tok.len
	return src[prev_tok_pos] == `\'`
}

fn (mut p Parser) detect_parse_mode() {
	src := p.scanner.text
	if src.len > 1 && src[0].is_digit() && !src[1].is_digit() {
		p.mode = .invalid
		return
	}
	p.tok = p.scanner.scan()
	p.n_tok = p.scanner.scan()
	if src.len == 1 && p.tok.kind == .string && p.n_tok.kind == .eof {
		p.mode = .invalid
		return
	}
	match p.tok.kind {
		.lcbr {
			p.mode = .object
		}
		.lsbr {
			p.mode = .array
		}
		.number {
			p.mode = .number
		}
		.key_true, .key_false {
			p.mode = .bool
		}
		.string {
			p.mode = .string
		}
		.name {
			if p.tok.lit == 'null' {
				p.mode = .null
			}
		}
		.minus {
			if p.n_tok.kind == .number {
				p.mode = .number
			}
		}
		else {}
	}
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
		.number {
			return p.decode_number()
		}
		.key_true {
			p.next()
			return if p.convert_type {
				Any(true)
			} else {
				Any('true')
			}
		}
		.key_false {
			p.next()
			return if p.convert_type {
				Any(false)
			} else {
				Any('false')
			}
		}
		.name {
			if p.tok.lit != 'null' {
				return error('unknown identifier `$p.tok.lit`')
			}
			p.next()
			return if p.convert_type {
				Any(Null{})
			} else {
				Any('null')
			}
		}
		.string {
			if p.is_singlequote() {
				return error('strings must be in double-quotes.')
			}
			return p.decode_string()
		}
		else {
			if p.tok.kind == .minus && p.n_tok.kind == .number && p.n_tok.pos == p.tok.pos + 1 {
				p.next()
				d_num := p.decode_number() ?
				return d_num
			}
			return error("unknown token '$p.tok.lit' when decoding value")
		}
	}
	if p.is_formfeed() {
		return error(formfeed_err)
	}
	return Any{}
}

fn (mut p Parser) decode_string() ?Any {
	mut strwr := strings.new_builder(200)
	for i := 0; i < p.tok.lit.len; i++ {
		if ((i - 1 >= 0 && p.tok.lit[i - 1] != `/`) || i == 0) && int(p.tok.lit[i]) in [9, 10, 0] {
			return error('character must be escaped with a backslash.')
		}
		if i == p.tok.lit.len - 1 && p.tok.lit[i] == 92 {
			return error('invalid backslash escape.')
		}
		if i + 1 < p.tok.lit.len && p.tok.lit[i] == 92 {
			peek := p.tok.lit[i + 1]
			match peek {
				`b` {
					i++
					strwr.write_b(`\b`)
					continue
				}
				`f` {
					i++
					strwr.write_b(`\f`)
					continue
				}
				`n` {
					i++
					strwr.write_b(`\n`)
					continue
				}
				`r` {
					i++
					strwr.write_b(`\r`)
					continue
				}
				`t` {
					i++
					strwr.write_b(`\t`)
					continue
				}
				`u` {
					if i + 5 < p.tok.lit.len {
						codepoint := p.tok.lit[i + 2..i + 6]
						check_valid_hex(codepoint) ?
						hex_val := strconv.parse_int(codepoint, 16, 0)
						strwr.write_b(byte(hex_val))
						i += 5
						continue
					} else {
						return error('incomplete unicode escape.')
					}
				}
				`\\` {
					i++
					strwr.write_b(`\\`)
					continue
				}
				`"` {
					i++
					strwr.write_b(`\"`)
					continue
				}
				`/` {
					i++
					strwr.write_b(`/`)
					continue
				}
				else { return error('invalid backslash escape.') }
			}
			if int(peek) == 85 {
				return error('unicode endpoints must be in lowercase `u`.')
			}
			if int(peek) in [9, 229] {
				return error('unicode endpoint not allowed.')
			}
		}
		strwr.write_b(p.tok.lit[i])
	}
	p.next()
	defer {
		strwr.free()
	}
	str := strwr.str()
	return Any(str)
}

// now returns string instead of int or float
fn (mut p Parser) decode_number() ?Any {
	src := p.scanner.text
	mut tl := p.tok.lit
	mut is_fl := false
	sep_by_dot := tl.to_lower().split('.')
	if tl.starts_with('0x') && tl.all_after('0x').len <= 2 {
		return error('hex numbers should not be less than or equal to two digits.')
	}
	if src[p.p_tok.pos + p.p_tok.len] == `0` && src[p.p_tok.pos + p.p_tok.len + 1].is_digit() {
		return error('leading zeroes in integers are not allowed.')
	}
	if tl.starts_with('.') {
		return error('decimals must start with a digit followed by a dot.')
	}
	if tl.ends_with('+') || tl.ends_with('-') {
		return error('exponents must have a digit before the sign.')
	}
	if sep_by_dot.len > 1 {
		// analyze json number structure
		// -[digit][dot][digit][E/e][-/+][digit]
		// float number
		is_fl = true
		last := sep_by_dot.last()
		if last.starts_with('e') {
			return error('exponents must have a digit before the exponent notation.')
		}
	}
	if p.p_tok.kind == .minus && p.tok.pos == p.p_tok.pos + 1 {
		tl = '-$tl'
	}
	p.next()
	if p.convert_type {
		return if is_fl {
			Any(tl.f64())
		} else {
			Any(tl.i64())
		}
	}
	return Any(tl)
}

fn (mut p Parser) decode_array() ?Any {
	mut items := []Any{}
	p.next()
	for p.tok.kind != .rsbr {
		if p.tok.kind == .eof {
			return error(eof_err)
		}
		item := p.decode_value() ?
		items << item
		if p.tok.kind == .comma && p.n_tok.kind !in [.rsbr, .comma] {
			p.next()
			continue
		}
		if p.tok.kind == .rsbr {
			break
		}
		return error("unknown token '$p.tok.lit' when decoding arrays.")
	}
	p.next()
	return Any(items)
}

fn (mut p Parser) decode_object() ?Any {
	mut fields := map[string]Any{}
	mut cur_key := ''
	p.next()
	for p.tok.kind != .rcbr {
		is_key := p.tok.kind == .string && p.n_tok.kind == .colon
		// todo
		// if p.is_formfeed() {
		// return error(formfeed_err)
		// }
		if p.tok.kind == .eof {
			return error(eof_err)
		}
		if p.is_singlequote() {
			return error('object keys must be in single quotes.')
		}
		if !is_key {
			return error("invalid token `$p.tok.lit`, expected \'string\'")
		}
		cur_key = p.tok.lit
		p.next()
		p.next()
		fields[cur_key] = p.decode_value() ?
		if p.tok.kind == .comma && p.n_tok.kind !in [.rcbr, .comma] {
			p.next()
			continue
		} else if p.tok.kind == .rcbr {
			break
		}
		return error("unknown token '$p.tok.lit' when decoding object.")
	}
	p.next()
	return Any(fields)
}
