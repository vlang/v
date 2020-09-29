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
	mut p := new_parser(src)
	p.detect_parse_mode()

	if p.mode == .invalid {
		return error(p.emit_error('Invalid JSON.'))
	}

	fi := p.decode_value() or {
		return error(p.emit_error(err))
	}

	if p.tok.kind != .eof {
		return error(p.emit_error('Unknown token `$p.tok.kind`.'))
	}

	return fi
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
// A generic function that encodes a type into a JSON string.
pub fn encode<T>(typ T) string {
	return typ.to_json()
}
// A simple function that returns `Null` struct. For use on constructing an `Any` object.
pub fn null() Null {
	return Null{}
}
// Use `Any` as a map.
pub fn (f Any) as_map() map[string]Any {
	mut mp := map[string]Any

	match f {
		map[string]Any {
			return *f
		}
		string {
			mp['0'] = f
			return mp
		}
		int {
			mp['0'] = f
			return mp
		}
		bool {
			mp['0'] = f
			return mp
		}
		f64 {
			mp['0'] = f
			return mp
		}
		Null {
			mp['0'] = f
			return mp
		}
		else {
			if typeof(f) == 'array_Any' {
				arr := f as []Any
				for i, fi in arr {
					mp[i.str()] = fi
				}

				return mp
			}

			return mp
		}
	}
}

// Use `Any` as an integer.
pub fn (f Any) int() int {
	match f {
		int  { return *f }
		f64  { return f.str().int() }
		else { return 0 }
	}
}
// Use `Any` as a float.
pub fn (f Any) f64() f64 {
	match f {
		int { return *f }
		f64 { return *f }
		else { return 0.0 }
	}
}
// Use `Any` as an array.
pub fn (f Any) arr() []Any {
	if f is []Any {
		return *f
	}

	if f is map[string]Any {
		mut arr := []Any{}
		mp := *f
		for _, v in mp {
			arr << v
		}
		return arr
	}

	return [f]
}

// Use `Any` as a bool
pub fn (f Any) bool() bool {
	match f {
		bool { return *f }
		string { return (*f).bool() }
		else { return false }
	}
}
