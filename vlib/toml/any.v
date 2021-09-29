// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module toml

import time

// Pretty much all the same builtin types as the `json2.Any` type plus `time.Time`
pub type Any = Null
	| []Any
	| bool
	| f32
	| f64
	| i64
	| int
	| map[string]Any
	| string
	| time.Time
	| u64

// string returns `Any` as a string.
pub fn (a Any) string() string {
	match a {
		string { return a as string }
		time.Time { return a.format_ss_micro() }
		else { return a.str() }
	}
}

// int returns `Any` as an 32-bit integer.
pub fn (a Any) int() int {
	match a {
		int { return a }
		i64, f32, f64, bool { return int(a) }
		// time.Time { return int(0) } // TODO
		else { return 0 }
	}
}

// i64 returns `Any` as a 64-bit integer.
pub fn (a Any) i64() i64 {
	match a {
		i64 { return a }
		int, f32, f64, bool { return i64(a) }
		// time.Time { return i64(0) } // TODO
		else { return 0 }
	}
}

// u64 returns `Any` as a 64-bit unsigned integer.
pub fn (a Any) u64() u64 {
	match a {
		u64 { return a }
		int, i64, f32, f64, bool { return u64(a) }
		// time.Time { return u64(0) } // TODO
		else { return 0 }
	}
}

// f32 returns `Any` as a 32-bit float.
pub fn (a Any) f32() f32 {
	match a {
		f32 { return a }
		int, i64, f64 { return f32(a) }
		// time.Time { return f32(0) } // TODO
		else { return 0.0 }
	}
}

// f64 returns `Any` as a 64-bit float.
pub fn (a Any) f64() f64 {
	match a {
		f64 { return a }
		int, i64, f32 { return f64(a) }
		// time.Time { return f64(0) } // TODO
		else { return 0.0 }
	}
}

// array returns `Any` as an array.
pub fn (a Any) array() []Any {
	if a is []Any {
		return a
	} else if a is map[string]Any {
		mut arr := []Any{}
		for _, v in a {
			arr << v
		}
		return arr
	}
	return [a]
}

// as_map returns `Any` as a map (TOML table).
pub fn (a Any) as_map() map[string]Any {
	if a is map[string]Any {
		return a
	} else if a is []Any {
		mut mp := map[string]Any{}
		for i, fi in a {
			mp['$i'] = fi
		}
		return mp
	}
	return {
		'0': a
	}
}

// bool returns `Any` as a boolean.
pub fn (a Any) bool() bool {
	match a {
		bool { return a }
		string { return a.bool() }
		else { return false }
	}
}

// date returns `Any` as a date encoded in a `time.Time` struct.
pub fn (a Any) date() time.Time {
	mut time := time.Time{}
	match a {
		// string {  } // TODO
		time.Time { return a }
		else { return time }
	}
}

// date returns `Any` as a time encoded in a `time.Time` struct.
pub fn (a Any) time() time.Time {
	mut time := time.Time{}
	match a {
		// string {  } // TODO
		time.Time { return a }
		else { return time }
	}
}

// date returns `Any` as a date+time encoded in a `time.Time` struct.
pub fn (a Any) datetime() time.Time {
	mut time := time.Time{}
	match a {
		// string {  } // TODO
		time.Time { return a }
		else { return time }
	}
}

pub fn (m map[string]Any) value(key string) ?Any {
	// return m[key] ?
	key_split := key.split('.')
	// util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, ' getting "${key_split[0]}"')
	if key_split[0] in m.keys() {
		value := m[key_split[0]] or {
			return error(@MOD + '.' + @STRUCT + '.' + @FN + ' key "$key" does not exist')
		}
		// `match` isn't currently very suitable for these types of sum type constructs...
		if value is map[string]Any {
			nm := (value as map[string]Any)
			next_key := key_split[1..].join('.')
			if next_key == '' {
				return value
			}
			return nm.value(next_key)
		}
		return value
	}
	return error(@MOD + '.' + @STRUCT + '.' + @FN + ' key "$key" does not exist')
}

pub fn (a []Any) as_strings() []string {
	mut sa := []string{}
	for any in a {
		sa << any.string()
	}
	return sa
}

// to_json returns `Any` as a JSON encoded string.
pub fn (a Any) to_json() string {
	match a {
		Null {
			return 'null'
		}
		string {
			return '"$a.str()"'
		}
		bool, f32, f64, i64, int, u64 {
			return a.str()
		}
		map[string]Any {
			mut str := '{'
			for key, val in a {
				str += ' "$key": $val.to_json(),'
			}
			str = str.trim_right(',')
			str += ' }'
			return str
		}
		[]Any {
			mut str := '['
			for val in a {
				str += ' $val.to_json(),'
			}
			str = str.trim_right(',')
			str += ' ]'
			return str
		}
		time.Time {
			return '"$a.format_ss_micro()"'
		}
	}
}
