// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

import time

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

// map_from convert a struct to map of Any
pub fn map_from[T](t T) map[string]Any {
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
				// TODO: improve memory usage when convert
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
