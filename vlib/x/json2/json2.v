// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

import strings
import time

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
	mut typ := T{}
	res := raw_decode(src)!.as_map()
	$if T is $struct {
		$for field in T.fields {
			mut json_name := field.name
			for attr in field.attrs {
				if attr.contains('json: ') {
					json_name = attr.replace('json: ', '')
					break
				}
			}

			$if field.is_enum {
				typ.$(field.name) = if key := res[field.name] {
					key.int()
				} else {
					res[json_name]!.int()
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
				typ.$(field.name) = res[field.name]!.to_time()!
			} $else $if field.typ is ?time.Time {
				if json_name in res {
					typ.$(field.name) = res[field.name]!.to_time()!
				}
			} $else $if field.is_array {
				// typed_arr_element := new_empty_element_from(typ.$(field.name))

				// for variable in res[field.name] or { []Any{} }.arr() {
				// 	$if typed_arr_element is string {
				// 		typ.$(field.name) << variable.str()
				// 	}
				// }
			} $else $if field.is_struct {
			} $else $if field.is_alias {
			} $else $if field.is_map {
			} $else {
				return error("The type of `${field.name}` can't be decoded. Please open an issue at https://github.com/vlang/v/issues/new/choose")
			}
		}
	} $else $if T is $map {
		for k, v in res {
			// // TODO - make this work to decode types like `map[string]StructType[bool]`
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

// new_empty_element_from usefull for `$if new_empty_element_from(typ.$(field.name)) is $Struct`
fn new_empty_element_from[T](t []T) T {
	return T{}
}

// encode is a generic function that encodes a type into a JSON string.
pub fn encode[T](val T) string {
	$if T is $array {
		return encode_array(val)
	} $else {
		mut sb := strings.new_builder(64)

		defer {
			unsafe { sb.free() }
		}

		default_encoder.encode_value(val, mut sb) or {
			dump(err)
			default_encoder.encode_value[Null](null, mut sb) or {}
		}

		return sb.str()
	}
}

// encode_array is a generic function that encodes a array into a JSON string.
fn encode_array[T](val []T) string {
	mut sb := strings.new_builder(64)

	defer {
		unsafe { sb.free() }
	}

	default_encoder.encode_array(val, 1, mut sb) or {
		dump(err)
		default_encoder.encode_value[Null](null, mut sb) or {}
	}

	return sb.str()
}

// encode_pretty ...
pub fn encode_pretty[T](typed_data T) string {
	encoded := encode(typed_data)
	raw_decoded := raw_decode(encoded) or { 0 }
	return raw_decoded.prettify_json_str()
}

// i8 - TODO
pub fn (f Any) i8() i8 {
	match f {
		i8 {
			return f
		}
		i16, int, i64, u8, u16, u32, u64, f32, f64, bool {
			return i8(f)
		}
		string {
			return f.i8()
		}
		else {
			return 0
		}
	}
}

// i16 - TODO
pub fn (f Any) i16() i16 {
	match f {
		i16 {
			return f
		}
		i8, int, i64, u8, u16, u32, u64, f32, f64, bool {
			return i16(f)
		}
		string {
			return f.i16()
		}
		else {
			return 0
		}
	}
}

// int uses `Any` as an integer.
pub fn (f Any) int() int {
	match f {
		int {
			return f
		}
		i8, i16, i64, u8, u16, u32, u64, f32, f64, bool {
			return int(f)
		}
		string {
			return f.int()
		}
		else {
			return 0
		}
	}
}

// i64 uses `Any` as a 64-bit integer.
pub fn (f Any) i64() i64 {
	match f {
		i64 {
			return f
		}
		i8, i16, int, u8, u16, u32, u64, f32, f64, bool {
			return i64(f)
		}
		string {
			return f.i64()
		}
		else {
			return 0
		}
	}
}

// u64 uses `Any` as a 64-bit unsigned integer.
pub fn (f Any) u64() u64 {
	match f {
		u64 {
			return f
		}
		u8, u16, u32, i8, i16, int, i64, f32, f64, bool {
			return u64(f)
		}
		string {
			return f.u64()
		}
		else {
			return 0
		}
	}
}

// f32 uses `Any` as a 32-bit float.
pub fn (f Any) f32() f32 {
	match f {
		f32 {
			return f
		}
		bool, i8, i16, int, i64, u8, u16, u32, u64, f64 {
			return f32(f)
		}
		string {
			return f.f32()
		}
		else {
			return 0.0
		}
	}
}

// f64 uses `Any` as a 64-bit float.
pub fn (f Any) f64() f64 {
	match f {
		f64 {
			return f
		}
		i8, i16, int, i64, u8, u16, u32, u64, f32 {
			return f64(f)
		}
		string {
			return f.f64()
		}
		else {
			return 0.0
		}
	}
}

// bool uses `Any` as a bool.
pub fn (f Any) bool() bool {
	match f {
		bool {
			return f
		}
		string {
			if f == 'false' {
				return false
			}
			if f == 'true' {
				return true
			}
			if f.len > 0 {
				return f != '0' && f != '0.0'
			} else {
				return false
			}
		}
		i8, i16, int, i64 {
			return i64(f) != 0
		}
		u8, u16, u32, u64 {
			return u64(f) != 0
		}
		f32, f64 {
			return f64(f) != 0.0
		}
		else {
			return false
		}
	}
}

// arr uses `Any` as an array.
pub fn (f Any) arr() []Any {
	if f is []Any {
		return f
	} else if f is map[string]Any {
		mut arr := []Any{}
		for _, v in f {
			arr << v
		}
		return arr
	}
	return [f]
}

// as_map uses `Any` as a map.
pub fn (f Any) as_map() map[string]Any {
	if f is map[string]Any {
		return f
	} else if f is []Any {
		mut mp := map[string]Any{}
		for i, fi in f {
			mp['${i}'] = fi
		}
		return mp
	}
	return {
		'0': f
	}
}

// to_time uses `Any` as a time.Time.
pub fn (f Any) to_time() !time.Time {
	match f {
		time.Time {
			return f
		}
		i64 {
			return time.unix(f)
		}
		string {
			if f.len == 10 && f[4] == `-` && f[7] == `-` {
				// just a date in the format `2001-01-01`
				return time.parse_iso8601(f)!
			}
			is_rfc3339 := f.len == 24 && f[23] == `Z` && f[10] == `T`
			if is_rfc3339 {
				return time.parse_rfc3339(f)!
			}
			mut is_unix_timestamp := true
			for c in f {
				if c == `-` || (c >= `0` && c <= `9`) {
					continue
				}
				is_unix_timestamp = false
				break
			}
			if is_unix_timestamp {
				return time.unix(f.i64())
			}
			// TODO - parse_iso8601
			// TODO - parse_rfc2822
			return time.parse(f)!
		}
		else {
			return error('not a time value: ${f} of type: ${f.type_name()}')
		}
	}
}

fn map_from[T](t T) map[string]Any {
	mut m := map[string]Any{}
	$if T is $struct {
		$for field in T.fields {
			value := t.$(field.name)

			$if field.is_array {
				mut arr := []Any{}
				for variable in value {
					arr << Any(variable)
				}
				m[field.name] = arr
				arr.clear()
			} $else $if field.is_struct {
				m[field.name] = map_from(value)
			} $else $if field.is_map {
				// TODO
			} $else $if field.is_alias {
				// TODO
			} $else $if field.is_option {
				// TODO
			} $else {
				// TODO improve memory usage when convert
				$if field.typ is string {
					m[field.name] = value.str()
				} $else $if field.typ is bool {
					m[field.name] = t.$(field.name).str().bool()
				} $else $if field.typ is i8 {
					m[field.name] = t.$(field.name).str().i8()
				} $else $if field.typ is i16 {
					m[field.name] = t.$(field.name).str().i16()
				} $else $if field.typ is int {
					m[field.name] = t.$(field.name).str().int()
				} $else $if field.typ is i64 {
					m[field.name] = t.$(field.name).str().i64()
				} $else $if field.typ is f32 {
					m[field.name] = t.$(field.name).str().f32()
				} $else $if field.typ is f64 {
					m[field.name] = t.$(field.name).str().f64()
				} $else $if field.typ is u8 {
					m[field.name] = t.$(field.name).str().u8()
				} $else $if field.typ is u16 {
					m[field.name] = t.$(field.name).str().u16()
				} $else $if field.typ is u32 {
					m[field.name] = t.$(field.name).str().u32()
				} $else $if field.typ is u64 {
					m[field.name] = t.$(field.name).str().u64()
				} $else {
					// return error("The type of `${field.name}` can't be decoded. Please open an issue at https://github.com/vlang/v/issues/new/choose")
				}
			}
		}
	}
	return m
}
