// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

import time

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
	scanner      &Scanner = unsafe { nil }
	prev_tok     Token
	tok          Token
	next_tok     Token
	n_level      int
	convert_type bool = true
}

fn (mut p Parser) next() {
	p.prev_tok = p.tok
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

// Decodes a JSON string into an `Any` type. Returns an option.
pub fn raw_decode(src string) !Any {
	mut p := new_parser(src, true)
	return p.decode()
}

// Same with `raw_decode`, but skips the type conversion for certain types when decoding a certain value.
pub fn fast_raw_decode(src string) !Any {
	mut p := new_parser(src, false)
	return p.decode()
}

// decode is a generic function that decodes a JSON string into the target type.
pub fn decode[T](src string) !T {
	res := raw_decode(src)!.as_map()
	return decode_struct[T](T{}, res)
}

// decode_struct is a generic function that decodes a JSON map into the struct T.
fn decode_struct[T](_ T, res map[string]Any) !T {
	mut typ := T{}
	$if T is $struct {
		$for field in T.fields {
			mut skip_field := false
			mut json_name := field.name

			for attr in field.attrs {
				if attr.contains('json: ') {
					json_name = attr.replace('json: ', '')
					if json_name == '-' {
						skip_field = true
					}
					break
				}
			}

			if !skip_field {
				$if field.is_enum {
					if v := res[json_name] {
						typ.$(field.name) = v.int()
					} else {
						$if field.is_option {
							typ.$(field.name) = none
						}
					}
				} $else $if field.typ is u8 {
					typ.$(field.name) = res[json_name]!.u64()
				} $else $if field.typ is u16 {
					typ.$(field.name) = res[json_name]!.u64()
				} $else $if field.typ is u32 {
					typ.$(field.name) = res[json_name]!.u64()
				} $else $if field.typ is u64 {
					typ.$(field.name) = res[json_name]!.u64()
				} $else $if field.typ is int {
					typ.$(field.name) = res[json_name]!.int()
				} $else $if field.typ is i8 {
					typ.$(field.name) = res[json_name]!.int()
				} $else $if field.typ is i16 {
					typ.$(field.name) = res[json_name]!.int()
				} $else $if field.typ is i32 {
					typ.$(field.name) = i32(res[field.name]!.int())
				} $else $if field.typ is i64 {
					typ.$(field.name) = res[json_name]!.i64()
				} $else $if field.typ is ?u8 {
					if json_name in res {
						typ.$(field.name) = ?u8(res[json_name]!.i64())
					}
				} $else $if field.typ is ?i8 {
					if json_name in res {
						typ.$(field.name) = ?i8(res[json_name]!.i64())
					}
				} $else $if field.typ is ?u16 {
					if json_name in res {
						typ.$(field.name) = ?u16(res[json_name]!.i64())
					}
				} $else $if field.typ is ?i16 {
					if json_name in res {
						typ.$(field.name) = ?i16(res[json_name]!.i64())
					}
				} $else $if field.typ is ?u32 {
					if json_name in res {
						typ.$(field.name) = ?u32(res[json_name]!.i64())
					}
				} $else $if field.typ is ?i32 {
					if json_name in res {
						typ.$(field.name) = ?i32(res[json_name]!.i64())
					}
				} $else $if field.typ is ?u64 {
					if json_name in res {
						typ.$(field.name) = ?u64(res[json_name]!.i64())
					}
				} $else $if field.typ is ?i64 {
					if json_name in res {
						typ.$(field.name) = ?i64(res[json_name]!.i64())
					}
				} $else $if field.typ is ?int {
					if json_name in res {
						typ.$(field.name) = ?int(res[json_name]!.i64())
					}
				} $else $if field.typ is f32 {
					typ.$(field.name) = res[json_name]!.f32()
				} $else $if field.typ is ?f32 {
					if json_name in res {
						typ.$(field.name) = res[json_name]!.f32()
					}
				} $else $if field.typ is f64 {
					typ.$(field.name) = res[json_name]!.f64()
				} $else $if field.typ is ?f64 {
					if json_name in res {
						typ.$(field.name) = res[json_name]!.f64()
					}
				} $else $if field.typ is bool {
					typ.$(field.name) = res[json_name]!.bool()
				} $else $if field.typ is ?bool {
					if json_name in res {
						typ.$(field.name) = res[json_name]!.bool()
					}
				} $else $if field.typ is string {
					typ.$(field.name) = res[json_name]!.str()
				} $else $if field.typ is ?string {
					if json_name in res {
						typ.$(field.name) = res[json_name]!.str()
					}
				} $else $if field.typ is time.Time {
					typ.$(field.name) = res[json_name]!.to_time()!
				} $else $if field.typ is ?time.Time {
					if json_name in res {
						typ.$(field.name) = res[json_name]!.to_time()!
					}
				} $else $if field.is_array {
					arr := res[field.name]! as []Any
					match typeof(typ.$(field.name)).name {
						'[]bool' { typ.$(field.name) = arr.map(it.bool()) }
						'[]?bool' { typ.$(field.name) = arr.map(?bool(it.bool())) }
						'[]f32' { typ.$(field.name) = arr.map(it.f32()) }
						'[]?f32' { typ.$(field.name) = arr.map(?f32(it.f32())) }
						'[]f64' { typ.$(field.name) = arr.map(it.f64()) }
						'[]?f64' { typ.$(field.name) = arr.map(?f64(it.f64())) }
						'[]i8' { typ.$(field.name) = arr.map(it.i8()) }
						'[]?i8' { typ.$(field.name) = arr.map(?i8(it.i8())) }
						'[]i16' { typ.$(field.name) = arr.map(it.i16()) }
						'[]?i16' { typ.$(field.name) = arr.map(?i16(it.i16())) }
						'[]i64' { typ.$(field.name) = arr.map(it.i64()) }
						'[]?i64' { typ.$(field.name) = arr.map(?i64(it.i64())) }
						'[]int' { typ.$(field.name) = arr.map(it.int()) }
						'[]?int' { typ.$(field.name) = arr.map(?int(it.int())) }
						'[]string' { typ.$(field.name) = arr.map(it.str()) }
						'[]?string' { typ.$(field.name) = arr.map(?string(it.str())) }
						// NOTE: Using `!` on `to_time()` inside the array method causes a builder error - 2024/04/01.
						'[]time.Time' { typ.$(field.name) = arr.map(it.to_time() or { time.Time{} }) }
						// vfmt off
						'[]?time.Time' { typ.$(field.name) = arr.map(?time.Time(it.to_time() or { time.Time{} })) }
						// vfmt on
						'[]u8' { typ.$(field.name) = arr.map(it.u64()) }
						'[]?u8' { typ.$(field.name) = arr.map(?u8(it.u64())) }
						'[]u16' { typ.$(field.name) = arr.map(it.u64()) }
						'[]?u16' { typ.$(field.name) = arr.map(?u16(it.u64())) }
						'[]u32' { typ.$(field.name) = arr.map(it.u64()) }
						'[]?u32' { typ.$(field.name) = arr.map(?u32(it.u64())) }
						'[]u64' { typ.$(field.name) = arr.map(it.u64()) }
						'[]?u64' { typ.$(field.name) = arr.map(?u64(it.u64())) }
						else {}
					}
				} $else $if field.is_struct {
					typ.$(field.name) = decode_struct(typ.$(field.name), res[field.name]!.as_map())!
				} $else $if field.is_alias {
				} $else $if field.is_map {
				} $else {
					return error("The type of `${field.name}` can't be decoded. Please open an issue at https://github.com/vlang/v/issues/new/choose")
				}
			}
		}
	} $else $if T is $map {
		for k, v in res {
			// // TODO: make this work to decode types like `map[string]StructType[bool]`
			// $if typeof(typ[k]).idx is string {
			// 	typ[k] = v.str()
			// } $else $if typeof(typ[k]).idx is $struct {

			// }
			match v {
				string {
					typ[k] = v.str()
				}
				else {}
			}
		}
	} $else {
		return error("The type `${T.name}` can't be decoded.")
	}
	return typ
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
		// `[`
		.lsbr {
			return p.decode_array()
		}
		// `{`
		.lcbr {
			return p.decode_object()
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
	return InvalidTokenError{
		token: p.tok
	}
}

@[manualfree]
fn (mut p Parser) decode_array() !Any {
	mut items := []Any{}
	p.next_with_err()!
	p.n_level++
	// `]`
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

fn (mut p Parser) decode_object() !Any {
	mut fields := map[string]Any{}
	p.next_with_err()!
	p.n_level++
	// `}`
	for p.tok.kind != .rcbr {
		// step 1 -> key
		if p.tok.kind != .str_ {
			return InvalidTokenError{
				token: p.tok
				expected: .str_
			}
		}

		cur_key := p.tok.lit.bytestr()
		p.next_with_err()!
		// step 2 -> colon separator
		if p.tok.kind != .colon {
			return InvalidTokenError{
				token: p.tok
				expected: .colon
			}
		}

		p.next_with_err()!
		// step 3 -> value
		fields[cur_key] = p.decode_value()!
		if p.tok.kind !in [.comma, .rcbr] {
			return InvalidTokenError{
				token: p.tok
				expected: .comma
			}
		} else if p.tok.kind == .comma {
			p.next_with_err()!
		}
	}
	p.next_with_err()!
	// step 4 -> eof (end)
	p.n_level--
	return Any(fields)
}
