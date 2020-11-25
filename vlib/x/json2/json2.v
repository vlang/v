// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module json2

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
// A generic function that decodes a JSON string into the target type.
//
// TODO: decode must return an optional generics
pub fn decode<T>(src string) T {
	res := raw_decode(src) or {
		panic(err)
	}
	mut typ := T{}
	typ.from_json(res)
	return typ
}
// TODO: decode must return an optional generics
// pub fn decode2<T>(src string) ?T {
// 	res := raw_decode(src)?

// 	match typeof(T) {
// 		'string' {
// 			return res.str()
// 		}
// 		'int' {
// 			return res.int()
// 		}
// 		'f64' {
// 			return res.f64()
// 		}
// 		else {
// 			mut typ := T{}
// 			typ.from_json(res)
// 			return typ
// 		}
// 	}
// }
// A generic function that encodes a type into a JSON string.
pub fn encode<T>(typ T) string {
	// if typeof(typ) in ['string', 'int', 'f64'] {
	// 	return Any(typ).str()
	// }

	return typ.to_json()
}
// A simple function that returns `Null` struct. For use on constructing an `Any` object.
pub fn null() Null {
	return Null{}
}
// Use `Any` as a map.
pub fn (f Any) as_map() map[string]Any {
	if f is map[string]Any {
		return f
	} else if f is []Any {
		mut mp := map[string]Any
		for i, fi in f {
			mp['$i'] = fi
		}
		return mp
	}
	return { '0': f }
}

// Use `Any` as an integer.
pub fn (f Any) int() int {
	match f {
		int  { return f }
		i64  { return int(f) }
		f64  { return f.str().int() }
		f32  { return f.str().int() }
		bool { return int(f) }
		else { return 0 }
	}
}

// Use `Any` as a 64-bit integer.
pub fn (f Any) i64() i64 {
	match f {
		int  { return f }
		i64  { return int(f) }
		f64  { return f.str().i64() }
		f32  { return f.str().i64() }
		bool { return int(f) }
		else { return 0 }
	}
}

// Use `Any` as a 32-bit float.
pub fn (f Any) f32() f32 {
	match f {
		int { return f }
		i64 { return f.str().f32() }
		f64 { return f.str().f32() }
		f32 { return f }
		else { return 0.0 }
	}
}

// Use `Any` as a float.
pub fn (f Any) f64() f64 {
	match f {
		int { return f }
		i64 { return f }
		f64 { return f }
		f32 { return f.str().f64() }
		else { return 0.0 }
	}
}
// Use `Any` as an array.
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

// Use `Any` as a bool
pub fn (f Any) bool() bool {
	match f {
		bool { return f }
		string { return f.bool() }
		else { return false }
	}
}
