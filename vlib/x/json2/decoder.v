// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

import time

fn format_message(msg string, line int, column int) string {
	return '[x.json2] ${msg} (${line}:${column})'
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
			line:    p.tok.line
			column:  p.tok.full_col()
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
		scanner:      &Scanner{
			text: src.bytes()
		}
		convert_type: convert_type
	}
}

// decode is a generic function that decodes a JSON string into the target type.
pub fn decode[T](src string) !T {
	$if T is Any {
		return raw_decode(src)!
	}
	res := raw_decode(src)!.as_map()
	return decode_struct[T](T{}, res)
}

// decode_struct_array is a generic function that decodes a JSON map into array struct T.
fn decode_struct_array[T](_ T, res map[string]Any) ![]T {
	$if T is $struct {
		mut arr := []T{}
		for v in res.values() {
			arr << decode_struct[T](T{}, v.as_map())!
		}
		return arr
	} $else {
		return error("The type `${T.name}` can't be decoded.")
	}
}

// decode_struct is a generic function that decodes a JSON map into the struct T.
fn decode_struct[T](_ T, res map[string]Any) !T {
	mut typ := T{}
	$if T is $struct {
		$for field in T.fields {
			mut skip_field := false
			mut json_name := field.name

			for attr in field.attrs {
				if attr.contains('skip') {
					skip_field = true
				}
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
					typ.$(field.name) = i32(res[field.name]!.i32())
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
				} $else $if field.typ is Any {
					typ.$(field.name) = res[json_name]!
				} $else $if field.typ is ?Any {
					if json_name in res {
						typ.$(field.name) = res[json_name]!
					}
				} $else $if field.is_array {
					arr := res[field.name]! as []Any
					decode_array_item(mut typ.$(field.name), arr)
				} $else $if field.is_struct {
					typ.$(field.name) = decode_struct(typ.$(field.name), res[field.name]!.as_map())!
				} $else $if field.is_alias {
				} $else $if field.is_map {
					typ.$(field.name) = decode_map(typ.$(field.name), res[field.name]!.as_map())!
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

fn decode_array_item[T](mut field T, arr []Any) {
	// vfmt off
	match typeof[T]().idx {
		typeof[[]bool]().idx  { field = arr.map(it.bool()) }
		typeof[[]?bool]().idx { field = arr.map(?bool(it.bool())) }
		typeof[[]f32]().idx   { field = arr.map(it.f32()) }
		typeof[[]?f32]().idx  { field = arr.map(?f32(it.f32())) }
		typeof[[]f64]().idx   { field = arr.map(it.f64()) }
		typeof[[]?f64]().idx  { field = arr.map(?f64(it.f64())) }
		typeof[[]i8]().idx    { field = arr.map(it.i8()) }
		typeof[[]?i8]().idx   { field = arr.map(?i8(it.i8())) }
		typeof[[]i16]().idx   { field = arr.map(it.i16()) }
		typeof[[]?i16]().idx  { field = arr.map(?i16(it.i16())) }
		typeof[[]i32]().idx   { field = arr.map(it.i32()) }
		typeof[[]?i32]().idx  { field = arr.map(?i32(it.i32())) }
		typeof[[]i64]().idx   { field = arr.map(it.i64()) }
		typeof[[]?i64]().idx  { field = arr.map(?i64(it.i64())) }
		typeof[[]int]().idx   { field = arr.map(it.int()) }
		typeof[[]?int]().idx  { field = arr.map(?int(it.int())) }
		typeof[[]string]().idx  { field = arr.map(it.str()) }
		typeof[[]?string]().idx { field = arr.map(?string(it.str())) }
		// NOTE: Using `!` on `to_time()` inside the array method causes a builder error - 2024/04/01.
		typeof[[]time.Time]().idx { field = arr.map(it.to_time() or { time.Time{} }) }
		typeof[[]?time.Time]().idx { field = arr.map(?time.Time(it.to_time() or { time.Time{} })) }
		typeof[[]Any]().idx { field = arr.clone() }
		typeof[[]?Any]().idx { field = arr.map(it) }
		typeof[[]u8]().idx   { field = arr.map(it.u64()) }
		typeof[[]?u8]().idx  { field = arr.map(?u8(it.u64())) }
		typeof[[]u16]().idx  { field = arr.map(it.u64()) }
		typeof[[]?u16]().idx { field = arr.map(?u16(it.u64())) }
		typeof[[]u32]().idx  { field = arr.map(it.u64()) }
		typeof[[]?u32]().idx { field = arr.map(?u32(it.u64())) }
		typeof[[]u64]().idx  { field = arr.map(it.u64()) }
		typeof[[]?u64]().idx { field = arr.map(?u64(it.u64())) }
		else {
			$if T is [][]f32 { field << arr.map(it.as_map().values().map(it.f32())) }
			$else $if T is [][]?f32 { field << arr.map(it.as_map().values().map(?f32(it.f32()))) }
			$else $if T is [][]f64  { field << arr.map(it.as_map().values().map(it.f64())) }
			$else $if T is [][]?f64 { field << arr.map(it.as_map().values().map(?f64(it.f64()))) }
			$else $if T is [][]i8   { field << arr.map(it.as_map().values().map(it.i8())) }
			$else $if T is [][]?i8  { field << arr.map(it.as_map().values().map(?i8(it.i8()))) }
			$else $if T is [][]i16  { field << arr.map(it.as_map().values().map(it.i16())) }
			$else $if T is [][]?i16 { field << arr.map(it.as_map().values().map(?i16(it.i16()))) }
			$else $if T is [][]i32  { field << arr.map(it.as_map().values().map(it.i32())) }
			$else $if T is [][]?i32 { field << arr.map(it.as_map().values().map(?i32(it.i32()))) }
			$else $if T is [][]i64  { field << arr.map(it.as_map().values().map(it.i64())) }
			$else $if T is [][]?i64 { field << arr.map(it.as_map().values().map(?i64(it.i64()))) }
			$else $if T is [][]u8   { field << arr.map(it.as_map().values().map(it.u8())) }
			$else $if T is [][]?u8  { field << arr.map(it.as_map().values().map(?u8(it.u8()))) }
			$else $if T is [][]u16  { field << arr.map(it.as_map().values().map(it.u16())) }
			$else $if T is [][]?u16 { field << arr.map(it.as_map().values().map(?u16(it.u16()))) }
			$else $if T is [][]u32  { field << arr.map(it.as_map().values().map(it.u32())) }
			$else $if T is [][]?u32 { field << arr.map(it.as_map().values().map(?u32(it.u32()))) }
			$else $if T is [][]u64  { field << arr.map(it.as_map().values().map(it.u64())) }
			$else $if T is [][]?u64 { field << arr.map(it.as_map().values().map(?u64(it.u64()))) }
			$else $if T is [][]bool { field << arr.map(it.as_map().values().map(it.bool())) }
			$else $if T is [][]?bool  { field << arr.map(it.as_map().values().map(?bool(it.bool()))) }
			$else $if T is [][]string  { field << arr.map(it.as_map().values().map(it.str())) }
			$else $if T is [][]?string { field << arr.map(it.as_map().values().map(?string(it.str()))) }
		}
	}
	// vfmt on
}

fn decode_map[K, V](_ map[K]V, res map[string]Any) !map[K]V {
	mut ret := map[K]V{}

	for k, v in res {
		$if V is $struct {
			ret[k] = decode_struct(V{}, res[k]!.as_map())!
		} $else $if V is $map {
			ret[k] = decode_map(V{}, res[k]!.as_map())!
		} $else $if V is $sumtype {
			ret[k] = decode_struct(V{}, res[k]!.as_map())!
		} $else $if V is $string {
			ret[k] = v.str()
		} $else $if V is int {
			ret[k] = v.int()
		} $else $if V is i64 {
			ret[k] = v.i64()
		} $else $if V is u64 {
			ret[k] = v.u64()
		} $else $if V is i32 {
			ret[k] = v.i32()
		} $else $if V is u32 {
			ret[k] = v.u32()
		} $else $if V is i16 {
			ret[k] = v.i16()
		} $else $if V is u16 {
			ret[k] = v.u16()
		} $else $if V is i8 {
			ret[k] = v.i8()
		} $else $if V is u8 {
			ret[k] = v.u8()
		} $else $if V is bool {
			ret[k] = v.bool()
		} $else {
			dump(v)
		}
	}
	return ret
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
		.int, .float {
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
		.bool {
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
		.str {
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
				kind:  .array
			}
		}
	}
	p.next_with_err()!
	p.n_level--
	return Any(items)
}

@[deprecated_after: '2025-03-18']
fn (mut p Parser) decode_object() !Any {
	mut fields := map[string]Any{}
	p.next_with_err()!
	p.n_level++
	// `}`
	for p.tok.kind != .rcbr {
		// step 1 -> key
		if p.tok.kind != .str {
			return InvalidTokenError{
				token:    p.tok
				expected: .str
			}
		}

		cur_key := p.tok.lit.bytestr()
		p.next_with_err()!
		// step 2 -> colon separator
		if p.tok.kind != .colon {
			return InvalidTokenError{
				token:    p.tok
				expected: .colon
			}
		}

		p.next_with_err()!
		// step 3 -> value
		fields[cur_key] = p.decode_value()!
		if p.tok.kind !in [.comma, .rcbr] {
			return InvalidTokenError{
				token:    p.tok
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
