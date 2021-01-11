// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
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
