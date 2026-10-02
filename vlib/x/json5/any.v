module json5

import strconv

// null is the `Any` value for a missing key, mirroring `toml.null`.
pub const null = Any(Null{})

// Any is the dynamic representation of a parsed JSON5 value. It mirrors the
// shape of `json2.Any`, but numbers keep their literal source text so that
// `Infinity`, `NaN` and hexadecimal literals survive a round-trip.
pub type Any = Null
	| Number
	| Raw
	| bool
	| f64
	| []Any
	| map[string]Any
	| string

// Raw is JSON5 text that the encoder writes verbatim. A type's `to_json5()`
// method produces a `Raw`, which is how a custom encoder can control quoting,
// comments and spacing.
pub struct Raw {
pub:
	text string
}

// str returns `Any` as JSON5 text.
pub fn (a Any) str() string {
	return encode_any(a, default_encode_opts)
}

// string returns `Any` as a string. Non-string scalars are converted.
pub fn (a Any) string() string {
	return match a {
		string { a }
		Number, Raw { a.text }
		Null { '' }
		bool {
			if a { 'true' } else { 'false' }
		}
		f64 { a.str() }
		else { '' }
	}
}

// int returns `Any` as an int.
pub fn (a Any) int() int {
	return match a {
		Number { a.int() }
		Raw {
			Number{
				text: a.text
			}.int()
		}
		f64 { int(a) }
		bool {
			if a { 1 } else { 0 }
		}
		string { strconv.atoi(a) or { 0 } }
		else { 0 }
	}
}

// i64 returns `Any` as an i64.
pub fn (a Any) i64() i64 {
	return match a {
		Number { a.i64() }
		Raw {
			Number{
				text: a.text
			}.i64()
		}
		f64 { i64(a) }
		bool {
			if a { 1 } else { 0 }
		}
		string { strconv.parse_int(a, 10, 64) or { 0 } }
		else { 0 }
	}
}

// u64 returns `Any` as a u64.
pub fn (a Any) u64() u64 {
	return match a {
		Number { a.u64() }
		Raw {
			Number{
				text: a.text
			}.u64()
		}
		f64 { u64(a) }
		bool {
			if a { 1 } else { 0 }
		}
		string { strconv.parse_uint(a, 10, 64) or { 0 } }
		else { 0 }
	}
}

// f64 returns `Any` as an f64.
pub fn (a Any) f64() f64 {
	return match a {
		Number { a.f64() }
		Raw {
			Number{
				text: a.text
			}.f64()
		}
		f64 { a }
		bool {
			if a { 1.0 } else { 0.0 }
		}
		string { strconv.atof64(a, strconv.AtoF64Param{}) or { 0.0 } }
		else { 0.0 }
	}
}

// bool returns `Any` as a bool.
pub fn (a Any) bool() bool {
	return match a {
		bool { a }
		Number { a.f64() != 0.0 }
		f64 { a != 0.0 }
		string { a.bool() }
		else { false }
	}
}

// array returns `Any` as an array. An object is returned as its values, and any
// other value as a single-element array.
pub fn (a Any) array() []Any {
	return match a {
		[]Any { a }
		map[string]Any { a.values() }
		else { [a] }
	}
}

// as_map returns `Any` as an object. An array is keyed by its indices and any
// other value becomes a single-entry object under the key `0`.
pub fn (a Any) as_map() map[string]Any {
	return match a {
		map[string]Any { a }
		[]Any {
			mut m := map[string]Any{}
			for i, item in a {
				m[i.str()] = item
			}
			m
		}
		else {
			{
				'0': a
			}
		}
	}
}

// default_to returns `value` if `a` is `Null`.
pub fn (a Any) default_to(value Any) Any {
	return match a {
		Null { value }
		else { a }
	}
}

// ValueKind names the JSON5 kinds of value.
pub enum ValueKind {
	bool
	number
	string
	null
	array
	object
}

// kind returns the JSON5 kind of `a`.
pub fn (a Any) kind() ValueKind {
	return match a {
		Null { ValueKind.null }
		bool { ValueKind.bool }
		Number, Raw, f64 { ValueKind.number }
		string { ValueKind.string }
		[]Any { ValueKind.array }
		map[string]Any { ValueKind.object }
	}
}

// str returns the JSON5 kind name, for error messages.
pub fn (k ValueKind) str() string {
	return match k {
		.bool { 'a boolean' }
		.number { 'a number' }
		.string { 'a string' }
		.null { '`null`' }
		.array { 'an array' }
		.object { 'an object' }
	}
}

// is_null reports whether `a` is the JSON5 `null` literal.
pub fn (a Any) is_null() bool {
	return a is Null
}

// is_number reports whether `a` is a number.
pub fn (a Any) is_number() bool {
	return a is Number
}

// is_string reports whether `a` is a string.
pub fn (a Any) is_string() bool {
	return a is string
}

// as_strings returns the array's elements as strings.
pub fn (a []Any) as_strings() []string {
	return a.map(it.string())
}

// as_strings returns the object's values as strings.
pub fn (m map[string]Any) as_strings() map[string]string {
	mut result := map[string]string{}
	for k, v in m {
		result[k] = v.string()
	}
	return result
}

// get returns the value stored under `key`, or none when it is absent.
pub fn (m map[string]Any) get(key string) ?Any {
	return m[key] or { none }
}

// has reports whether `m` contains `key`.
pub fn (m map[string]Any) has(key string) bool {
	return key in m
}
