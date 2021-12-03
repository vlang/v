// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module toml

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
		DateTime { return a.str().clone() }
		Date { return a.str().clone() }
		Time { return a.str().clone() }
		else { return a.str().clone() }
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
		// NOTE `.clone()` is to avoid memory corruption see `pub fn (a Any) string() string`
		Date { return Date{a.str().clone()} }
		else { return Date{''} }
	}
}

// time returns `Any` as a `toml.Time` struct.
pub fn (a Any) time() Time {
	match a {
		// string {  } // TODO
		// NOTE `.clone()` is to avoid memory corruption see `pub fn (a Any) string() string`
		Time { return Time{a.str().clone()} }
		else { return Time{''} }
	}
}

// datetime returns `Any` as a `toml.DateTime` struct.
pub fn (a Any) datetime() DateTime {
	match a {
		// string {  } // TODO
		// NOTE `.clone()` is to avoid memory corruption see `pub fn (a Any) string() string`
		DateTime { return DateTime{a.str().clone()} }
		else { return DateTime{''} }
	}
}

// default_to returns `value` if `a Any` is `Null`.
// This can be used to set default values when retrieving
// values. E.g.: `toml_doc.value('wrong.key').default_to(123).int()`
pub fn (a Any) default_to(value Any) Any {
	match a {
		Null { return value }
		else { return a }
	}
}

// value queries a value from the map.
// `key` supports a small query syntax scheme:
// Maps can be queried in "dotted" form e.g. `a.b.c`.
// quoted keys are supported as `a."b.c"` or `a.'b.c'`.
// Arrays can be queried with `a[0].b[1].[2]`.
pub fn (m map[string]Any) value(key string) Any {
	return Any(m).value(key)
}

// value queries a value from the array.
// `key` supports a small query syntax scheme:
// The array can be queried with `[0].b[1].[2]`.
// Maps can be queried in "dotted" form e.g. `a.b.c`.
// quoted keys are supported as `a."b.c"` or `a.'b.c'`.
pub fn (a []Any) value(key string) Any {
	return Any(a).value(key)
}

pub fn (a []Any) as_strings() []string {
	mut sa := []string{}
	for any in a {
		sa << any.string()
	}
	return sa
}

// value queries a value from the `Any` type.
// `key` supports a small query syntax scheme:
// Maps can be queried in "dotted" form e.g. `a.b.c`.
// quoted keys are supported as `a."b.c"` or `a.'b.c'`.
// Arrays can be queried with `a[0].b[1].[2]`.
pub fn (a Any) value(key string) Any {
	key_split := parse_dotted_key(key) or { return Any(Null{}) }
	return a.value_(a, key_split)
}

// value_ returns the `Any` value found at `key`.
fn (a Any) value_(value Any, key []string) Any {
	assert key.len > 0
	mut any_value := Any(Null{})
	k, index := parse_array_key(key[0])
	if k == '' {
		arr := value as []Any
		any_value = arr[index] or { return Any(Null{}) }
	}
	if value is map[string]Any {
		any_value = value[k] or { return Any(Null{}) }
		if index > -1 {
			arr := any_value as []Any
			any_value = arr[index] or { return Any(Null{}) }
		}
	}
	if key.len <= 1 {
		return any_value
	}
	match any_value {
		map[string]Any, []Any {
			return a.value_(any_value, key[1..])
		}
		else {
			return value
		}
	}
}

pub fn (a Any) reflect<T>() T {
	mut reflected := T{}
	$for field in T.fields {
		$if field.typ is string {
			reflected.$(field.name) = a.value(field.name).default_to('').string()
		} $else $if field.typ is bool {
			reflected.$(field.name) = a.value(field.name).default_to(false).bool()
		} $else $if field.typ is int {
			reflected.$(field.name) = a.value(field.name).default_to(0).int()
		} $else $if field.typ is f32 {
			reflected.$(field.name) = a.value(field.name).default_to(0.0).f32()
		} $else $if field.typ is f64 {
			reflected.$(field.name) = a.value(field.name).default_to(0.0).f64()
		} $else $if field.typ is i64 {
			reflected.$(field.name) = a.value(field.name).default_to(0).i64()
		} $else $if field.typ is u64 {
			reflected.$(field.name) = a.value(field.name).default_to(0).u64()
		} $else $if field.typ is Any {
			reflected.$(field.name) = a.value(field.name)
		} $else $if field.typ is DateTime {
			dt := DateTime{'0000-00-00T00:00:00.000'}
			reflected.$(field.name) = a.value(field.name).default_to(dt).datetime()
		} $else $if field.typ is Date {
			da := Date{'0000-00-00'}
			reflected.$(field.name) = a.value(field.name).default_to(da).date()
		} $else $if field.typ is Time {
			t := Time{'00:00:00.000'}
			reflected.$(field.name) = a.value(field.name).default_to(t).time()
		}
	}
	return reflected
}
