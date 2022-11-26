// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

import strings

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
	res := raw_decode(src)!
	mut typ := T{}
	typ.from_json(res)
	return typ
}

// encode is a generic function that encodes a type into a JSON string.
pub fn encode[T](val T) string {
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

// int uses `Any` as an integer.
pub fn (f Any) int() int {
	match f {
		int { return f }
		i8, i16, i64, u8, u16, u32, u64, f32, f64, bool { return int(f) }
		else { return 0 }
	}
}

// i64 uses `Any` as a 64-bit integer.
pub fn (f Any) i64() i64 {
	match f {
		i64 { return f }
		i8, i16, int, u8, u16, u32, u64, f32, f64, bool { return i64(f) }
		else { return 0 }
	}
}

// u64 uses `Any` as a 64-bit unsigned integer.
pub fn (f Any) u64() u64 {
	match f {
		u64 { return f }
		u8, u16, u32, i8, i16, int, i64, f32, f64, bool { return u64(f) }
		else { return 0 }
	}
}

// f32 uses `Any` as a 32-bit float.
pub fn (f Any) f32() f32 {
	match f {
		f32 { return f }
		bool, i8, i16, int, i64, u8, u16, u32, u64, f64 { return f32(f) }
		else { return 0.0 }
	}
}

// f64 uses `Any` as a 64-bit float.
pub fn (f Any) f64() f64 {
	match f {
		f64 { return f }
		i8, i16, int, i64, u8, u16, u32, u64, f32 { return f64(f) }
		else { return 0.0 }
	}
}

// bool uses `Any` as a bool.
pub fn (f Any) bool() bool {
	match f {
		bool { return f }
		string { return f.bool() }
		i8, i16, int, i64 { return i64(f) != 0 }
		u8, u16, u32, u64 { return u64(f) != 0 }
		f32, f64 { return f64(f) != 0.0 }
		else { return false }
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
