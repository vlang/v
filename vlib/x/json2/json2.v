// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

pub const (
	null = Null{}
)

pub interface Serializable {
	from_json(f Any)
	to_json() string
}

// Decodes a JSON string into an `Any` type. Returns an option.
pub fn raw_decode(src string) ?Any {
	mut p := new_parser(src, true)
	return p.decode()
}

// Same with `raw_decode`, but skips the type conversion for certain types when decoding a certain value.
pub fn fast_raw_decode(src string) ?Any {
	mut p := new_parser(src, false)
	return p.decode()
}

pub fn raw_encode(src Any) string {
	return src.json_str()
}

// decode is a generic function that decodes a JSON string into the target type.
pub fn decode<T>(src string) ?T {
	res := raw_decode(src) ?
	mut typ := T{}
	typ.from_json(res)
	return typ
}

// encode is a generic function that encodes a type into a JSON string.
pub fn encode<T>(typ T) string {
	return typ.to_json()
}

// as_map uses `Any` as a map.
pub fn (f Any) as_map() map[string]Any {
	if f is map[string]Any {
		return f
	} else if f is []Any {
		mut mp := map[string]Any{}
		for i, fi in f {
			mp['$i'] = fi
		}
		return mp
	}
	return {
		'0': f
	}
}

// int uses `Any` as an integer.
pub fn (f Any) int() int {
	match f {
		int { return f }
		i64, f32, f64, bool { return int(f) }
		else { return 0 }
	}
}

// i64 uses `Any` as a 64-bit integer.
pub fn (f Any) i64() i64 {
	match f {
		i64 { return f }
		int, f32, f64, bool { return i64(f) }
		else { return 0 }
	}
}

// u64 uses `Any` as a 64-bit unsigned integer.
pub fn (f Any) u64() u64 {
	match f {
		u64 { return f }
		int, i64, f32, f64, bool { return u64(f) }
		else { return 0 }
	}
}

// f32 uses `Any` as a 32-bit float.
pub fn (f Any) f32() f32 {
	match f {
		f32 { return f }
		int, i64, f64 { return f32(f) }
		else { return 0.0 }
	}
}

// f64 uses `Any` as a float.
pub fn (f Any) f64() f64 {
	match f {
		f64 { return f }
		int, i64, f32 { return f64(f) }
		else { return 0.0 }
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

// bool uses `Any` as a bool
pub fn (f Any) bool() bool {
	match f {
		bool { return f }
		string { return f.bool() }
		else { return false }
	}
}

pub fn (f Any) get(name string) ?Any {
	match f {
		map[string]Any {
			return f[name] or { return none }
		}
		[]Any {
			for i in 0 .. f.len {
				if i.str() == name {
					return f[i]
				}
			}
			return none
		}
		else {
			if name == '0' {
				return f
			}
			return none
		}
	}
}

fn decode_int(root Any) int {
	if root is Null {
		return 0
	}
	return root.int()
}

fn decode_i8(root Any) i8 {
	if root is Null {
		return i8(0)
	}
	return i8(root.int())
}

fn decode_i16(root Any) i16 {
	if root is Null {
		return i16(0)
	}
	return i16(root.int())
}

fn decode_i64(root Any) i64 {
	if root is Null {
		return i64(0)
	}
	return i64(root.f64()) // i64 is double in C
}

fn decode_byte(root Any) byte {
	if root is Null {
		return byte(0)
	}
	return byte(root.int())
}

fn decode_u8(root Any) u8 {
	if root is Null {
		return byte(0)
	}
	return byte(root.int())
}

fn decode_u16(root Any) u16 {
	if root is Null {
		return u16(0)
	}
	return u16(root.int())
}

fn decode_u32(root Any) u32 {
	if root is Null {
		return u32(0)
	}
	return u32(root.int())
}

fn decode_u64(root Any) u64 {
	if root is Null {
		return u64(0)
	}
	return u64(root.f64())
}

fn decode_f32(root Any) f32 {
	if root is Null {
		return f32(0)
	}
	return f32(root.f64())
}

fn decode_f64(root Any) f64 {
	if root is Null {
		return f64(0)
	}
	return root.f64()
}

fn decode_rune(root Any) rune {
	if root is Null {
		return rune(0)
	}
	if root !is string {
		return rune(0)
	}

	return root.str().runes().first()
}

fn decode_string(root Any) string {
	if root is Null {
		return ''
	}
	if root !is string {
		return ''
	}
	// println('decode string valuestring="$root.valuestring"')
	// return tos(root.valuestring, _strlen(root.valuestring))
	return root.str()
}

fn decode_bool(root Any) bool {
	if root is Null {
		return false
	}
	return root.bool()
}

// ///////////////////
fn encode_int(val int) Any {
	return Any(val)
}

fn encode_i8(val i8) Any {
	return Any(int(val))
}

fn encode_i16(val i16) Any {
	return Any(int(val))
}

fn encode_i64(val i64) Any {
	return Any(val)
}

fn encode_byte(val byte) Any {
	return Any(int(val))
}

fn encode_u8(val u8) Any {
	return Any(int(val))
}

fn encode_u16(val u16) Any {
	return Any(int(val))
}

fn encode_u32(val u32) Any {
	return Any(u64(val))
}

fn encode_u64(val u64) Any {
	return Any(val)
}

fn encode_f32(val f32) Any {
	return Any(val)
}

fn encode_f64(val f64) Any {
	return Any(val)
}

fn encode_bool(val bool) Any {
	return Any(val)
}

fn encode_rune(val rune) Any {
	return Any(val.str())
}

fn encode_string(val string) Any {
	return Any(val)
}
