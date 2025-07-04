// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

import time

// i8 uses `Any` as a 16-bit integer.
pub fn (f Any) i8() i8 {
	match f {
		i8 {
			return f
		}
		i16, i32, int, i64, u8, u16, u32, u64, f32, f64, bool {
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

// i16 uses `Any` as a 16-bit integer.
pub fn (f Any) i16() i16 {
	match f {
		i16 {
			return f
		}
		i8, i32, int, i64, u8, u16, u32, u64, f32, f64, bool {
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
		i8, i16, i32, i64, u8, u16, u32, u64, f32, f64, bool {
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

// i32 uses `Any` as a 32-bit integer.
pub fn (f Any) i32() i32 {
	match f {
		i32 {
			return f
		}
		i8, i16, int, i64, u8, u16, u32, u64, f32, f64, bool {
			return i32(f)
		}
		string {
			return f.i32()
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
		i8, i16, i32, int, u8, u16, u32, u64, f32, f64, bool {
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

// u8 uses `Any` as a 8-bit unsigned integer.
pub fn (f Any) u8() u8 {
	match f {
		u8 {
			return f
		}
		u16, u32, u64, i8, i16, i32, int, i64, f32, f64, bool {
			return u8(u16(f))
		}
		string {
			return f.u8()
		}
		else {
			return 0
		}
	}
}

// u16 uses `Any` as a 16-bit unsigned integer.
pub fn (f Any) u16() u16 {
	match f {
		u16 {
			return f
		}
		u8, u32, u64, i8, i16, i32, int, i64, f32, f64, bool {
			return u16(f)
		}
		string {
			return f.u16()
		}
		else {
			return 0
		}
	}
}

// u32 uses `Any` as a 32-bit unsigned integer.
pub fn (f Any) u32() u32 {
	match f {
		u32 {
			return f
		}
		u8, u16, u64, i8, i16, i32, int, i64, f32, f64, bool {
			return u32(f)
		}
		string {
			return f.u32()
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
		u8, u16, u32, i8, i16, i32, int, i64, f32, f64, bool {
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
		bool, i8, i16, i32, int, i64, u8, u16, u32, u64, f64 {
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
		i8, i16, i32, int, i64, u8, u16, u32, u64, f32 {
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
		i8, i16, i32, int, i64 {
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

pub fn (f Any) as_map_of_strings() map[string]string {
	if f is map[string]Any {
		mut ms := map[string]string{}
		for k, v in f {
			ms[k] = v.str()
		}
		return ms
	}
	if f is []Any {
		mut ms := map[string]string{}
		for i, fi in f {
			ms['${i}'] = fi.str()
		}
		return ms
	}
	return {
		'0': f.str()
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
			is_iso8601 := f[4] == `-` && f[7] == `-`
			if is_iso8601 {
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
			// TODO: parse_iso8601
			// TODO: parse_rfc2822
			return time.parse(f)!
		}
		else {
			return error('not a time value: ${f} of type: ${f.type_name()}')
		}
	}
}

// map_from converts a struct to a map of Any.
pub fn map_from[T](t T) map[string]Any {
	mut m := map[string]Any{}
	$if T is $struct {
		$for field in T.fields {
			value := t.$(field.name)

			$if field.is_array {
				mut top_arr := unsafe { []Any{len: t.$(field.name).len} }
				for idx, variable in value {
					$if variable is $array {
						top_arr[idx] = flatten_array(variable)
					} $else {
						top_arr[idx] = variable
					}
				}
				m[field.name] = top_arr
				top_arr.clear()
			} $else $if field.is_struct {
				m[field.name] = map_from(value)
			} $else $if field.is_map {
				// TODO
			} $else $if field.is_alias {
				// TODO
			} $else $if field.is_option {
				// TODO
			} $else {
				// TODO: simplify check of type & improve memory usage when convert
				$if field.typ is string {
					m[field.name] = value
				} $else $if field.typ is bool {
					m[field.name] = t.$(field.name)
				} $else $if field.typ is i8 {
					m[field.name] = t.$(field.name)
				} $else $if field.typ is i16 {
					m[field.name] = t.$(field.name)
				} $else $if field.typ is i32 {
					m[field.name] = t.$(field.name)
				} $else $if field.typ is int {
					m[field.name] = t.$(field.name)
				} $else $if field.typ is i64 {
					m[field.name] = t.$(field.name)
				} $else $if field.typ is f32 {
					m[field.name] = t.$(field.name)
				} $else $if field.typ is f64 {
					m[field.name] = t.$(field.name)
				} $else $if field.typ is u8 {
					m[field.name] = t.$(field.name)
				} $else $if field.typ is u16 {
					m[field.name] = t.$(field.name)
				} $else $if field.typ is u32 {
					m[field.name] = t.$(field.name)
				} $else $if field.typ is u64 {
					m[field.name] = t.$(field.name)
				} $else {
					// return error("The type of `${field.name}` can't be decoded. Please open an issue at https://github.com/vlang/v/issues/new/choose")
				}
			}
		}
	}
	return m
}

// flatten_array convert a array of values to array of Any
@[inline]
fn flatten_array[T](t []T) []Any {
	$if t !is $array {
		return Any(t)
	} $else {
		mut arr := []Any{}
		for variable in t {
			$if variable is $array {
				arr << flatten_array(variable)
			} $else {
				arr << Any(variable)
			}
		}
		return arr
	}
}
