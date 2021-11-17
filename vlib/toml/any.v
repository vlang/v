// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module toml

import toml.util
import x.json2

// Pretty much all the same builtin types as the `json2.Any` type plus `DateTime`,`Date`,`Time`
pub type Any = Date
	| DateTime
	| Null
	| Time
	| []Any
	| bool
	| f32
	| f64
	| i64
	| int
	| map[string]Any
	| string
	| u64

// string returns `Any` as a string.
pub fn (a Any) string() string {
	match a {
		// NOTE if `.clone()` is not used here:
		// string { return a as string }
		// ... certain call-patterns to this function will cause a memory corruption.
		// See `tests/toml_memory_corruption_test.v` for a matching regression test.
		string { return (a as string).clone() }
		DateTime { return a.datetime.clone() }
		Date { return a.date.clone() }
		Time { return a.time.clone() }
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

// date returns `Any` as a `toml.Date` struct.
pub fn (a Any) date() Date {
	match a {
		// string {  } // TODO
		Date { return a }
		else { return Date{''} }
	}
}

// time returns `Any` as a `toml.Time` struct.
pub fn (a Any) time() Time {
	match a {
		// string {  } // TODO
		Time { return a }
		else { return Time{''} }
	}
}

// datetime returns `Any` as a `toml.DateTime` struct.
pub fn (a Any) datetime() DateTime {
	match a {
		// string {  } // TODO
		DateTime { return a }
		else { return DateTime{''} }
	}
}

// value queries a value from the map.
// `key` should be in "dotted" form (`a.b.c`).
// `key` supports quoted keys like `a."b.c"`.
pub fn (m map[string]Any) value(key string) ?Any {
	key_split := util.parse_dotted_key(key) ?
	return m.value_(key_split)
}

fn (m map[string]Any) value_(key []string) ?Any {
	value := m[key[0]] or {
		return error(@MOD + '.' + @STRUCT + '.' + @FN + ' key "${key[0]}" does not exist')
	}
	// `match` isn't currently very suitable for these types of sum type constructs...
	if value is map[string]Any {
		if key.len <= 1 {
			return value
		}
		nm := (value as map[string]Any)
		return nm.value_(key[1..])
	}
	return value
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
		DateTime {
			json_text := json2.Any(a.str())
			return '"$json_text.json_str()"'
		}
		Date {
			json_text := json2.Any(a.str())
			return '"$json_text.json_str()"'
		}
		Time {
			json_text := json2.Any(a.str())
			return '"$json_text.json_str()"'
		}
		string {
			json_text := json2.Any(a.str())
			return '"$json_text.json_str()"'
		}
		bool, f32, f64, i64, int, u64 {
			json_text := json2.Any(a.str())
			return json_text.json_str()
		}
		map[string]Any {
			mut str := '{'
			for key, val in a {
				json_key := json2.Any(key)
				str += ' "$json_key.json_str()": $val.to_json(),'
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
	}
}

// to_json_any returns `Any` as a `x.json2.Any` type.
pub fn (a Any) to_json_any() json2.Any {
	match a {
		Null {
			return json2.Null{}
		}
		DateTime {
			return json2.Any(a.str())
		}
		Date {
			return json2.Any(a.str())
		}
		Time {
			return json2.Any(a.str())
		}
		string {
			return json2.Any(a.str())
		}
		bool {
			return json2.Any(bool(a))
		}
		int {
			return json2.Any(int(a))
		}
		f32 {
			return json2.Any(f32(a))
		}
		f64 {
			return json2.Any(f64(a))
		}
		i64 {
			return json2.Any(i64(a))
		}
		u64 {
			return json2.Any(u64(a))
		}
		map[string]Any {
			mut jmap := map[string]json2.Any{}
			for key, val in a {
				jmap[key] = val.to_json_any()
			}
			return jmap
		}
		[]Any {
			mut jarr := []json2.Any{}

			for val in a {
				jarr << val.to_json_any()
			}

			return jarr
		}
	}
}
