module yaml

import json
import os
import strings
import x.json2

// Null is a simple representation of the YAML `null` value.
pub struct Null {}

// null is an instance of `Null`, to ease comparisons with it.
pub const null = Any(Null{})

// Any is the tree representation used by the YAML module.
pub type Any = []Any | Null | bool | f64 | i64 | int | map[string]Any | string | u64

// Doc is a parsed YAML document.
pub struct Doc {
pub:
	root Any
}

// parse_file parses the YAML file at `path`.
pub fn parse_file(path string) !Doc {
	return parse_text(os.read_file(path)!)
}

// parse_text parses the YAML document provided in `text`.
pub fn parse_text(text string) !Doc {
	mut normalized := text
	if normalized.contains_u8(`\r`) {
		normalized = normalized.replace('\r\n', '\n').replace('\r', '\n')
	}
	if normalized.len >= 3 && normalized[0] == 0xef && normalized[1] == 0xbb
		&& normalized[2] == 0xbf {
		normalized = normalized[3..]
	}
	// `split('\n')` would otherwise turn the canonical trailing line break into
	// a phantom empty last line, which the block-scalar reader then treats as a
	// genuine blank line and over-counts during chomping.
	if normalized.ends_with('\n') {
		normalized = normalized[..normalized.len - 1]
	}
	trimmed := normalized.trim_space()
	if trimmed == '' {
		return Doc{
			root: null
		}
	}
	if trimmed.starts_with('{') || trimmed.starts_with('[') {
		// JSON-superset fast path. `parse_flow_value` already consumes the
		// flow-style grammar that YAML borrows from JSON, so it builds the
		// `yaml.Any` tree directly — no second-pass `from_json2` rebuild.
		// Falls through to the block parser if the body is anything other
		// than a clean flow document.
		if val := parse_flow_value(trimmed) {
			return Doc{
				root: val
			}
		}
	}
	mut parser := Parser{
		lines: normalized.split('\n')
	}
	return Doc{
		root: parser.parse()!
	}
}

// decode decodes YAML text into the target type `T`.
// The generic encode/decode path uses the main `json` module for field parity.
pub fn decode[T](yaml_text string) !T {
	doc := parse_text(yaml_text)!
	return doc.decode[T]()
}

// decode_file decodes the YAML file at `path` into the target type `T`.
pub fn decode_file[T](path string) !T {
	return decode[T](os.read_file(path)!)
}

// encode encodes the value `value` into a YAML string.
// The generic encode/decode path uses the main `json` module for field parity.
pub fn encode[T](value T) string {
	json_text := json.encode(value)
	raw := json2.decode[json2.Any](json_text) or { return '' }
	return from_json2(raw).to_yaml()
}

// encode_file encodes `value` as YAML and writes it to `path`.
pub fn encode_file[T](path string, value T) ! {
	os.write_file(path, encode(value))!
}

// decode decodes the YAML document into the target type `T`.
pub fn (d Doc) decode[T]() !T {
	return json.decode(T, d.to_json())!
}

// to_any converts the YAML document to `yaml.Any`.
pub fn (d Doc) to_any() Any {
	return d.root
}

// to_json converts the YAML document to JSON.
pub fn (d Doc) to_json() string {
	return d.root.to_json()
}

// to_yaml converts the YAML document back to YAML text.
pub fn (d Doc) to_yaml() string {
	return d.root.to_yaml()
}

// value queries a value from the YAML document.
// `key` supports dotted keys and array indexing like `servers[0].host`.
pub fn (d Doc) value(key string) Any {
	return d.root.value(key)
}

// value_opt queries a value from the YAML document and returns an error when missing.
pub fn (d Doc) value_opt(key string) !Any {
	return d.root.value_opt(key)
}

// str returns a display-friendly string form of `Any`.
pub fn (a Any) str() string {
	return a.string()
}

// string returns `Any` as a string when possible, or a YAML representation otherwise.
pub fn (a Any) string() string {
	return match a {
		string { a }
		bool, f64, i64, int, u64 { a.str() }
		Null { 'null' }
		[]Any, map[string]Any { a.to_yaml() }
	}
}

// int returns `Any` as an `int`.
pub fn (a Any) int() int {
	return match a {
		int {
			a
		}
		i64 {
			int(a)
		}
		u64 {
			int(a)
		}
		f64 {
			int(a)
		}
		bool {
			if a {
				1
			} else {
				0
			}
		}
		string {
			a.int()
		}
		else {
			0
		}
	}
}

// i64 returns `Any` as an `i64`.
pub fn (a Any) i64() i64 {
	return match a {
		i64 {
			a
		}
		int {
			i64(a)
		}
		u64 {
			i64(a)
		}
		f64 {
			i64(a)
		}
		bool {
			if a {
				i64(1)
			} else {
				i64(0)
			}
		}
		string {
			a.i64()
		}
		else {
			i64(0)
		}
	}
}

// u64 returns `Any` as a `u64`.
pub fn (a Any) u64() u64 {
	return match a {
		u64 {
			a
		}
		int {
			u64(a)
		}
		i64 {
			u64(a)
		}
		f64 {
			u64(a)
		}
		bool {
			if a {
				u64(1)
			} else {
				u64(0)
			}
		}
		string {
			a.u64()
		}
		else {
			u64(0)
		}
	}
}

// f64 returns `Any` as an `f64`.
pub fn (a Any) f64() f64 {
	return match a {
		f64 {
			a
		}
		int {
			f64(a)
		}
		i64 {
			f64(a)
		}
		u64 {
			f64(a)
		}
		bool {
			if a {
				1.0
			} else {
				0.0
			}
		}
		string {
			a.f64()
		}
		else {
			0.0
		}
	}
}

// bool returns `Any` as a `bool`.
pub fn (a Any) bool() bool {
	return match a {
		bool {
			a
		}
		int {
			a != 0
		}
		i64 {
			a != 0
		}
		u64 {
			a != 0
		}
		f64 {
			a != 0.0
		}
		string {
			lower := a.to_lower()
			lower in ['true', 'yes', 'on', '1']
		}
		else {
			false
		}
	}
}

// array returns `Any` as an array.
pub fn (a Any) array() []Any {
	return match a {
		[]Any {
			a
		}
		map[string]Any {
			mut arr := []Any{cap: a.len}
			for _, value in a {
				arr << value
			}
			arr
		}
		else {
			[a]
		}
	}
}

// as_map returns `Any` as a map.
pub fn (a Any) as_map() map[string]Any {
	return match a {
		map[string]Any {
			a
		}
		[]Any {
			mut out := map[string]Any{}
			for i, value in a {
				out['${i}'] = value
			}
			out
		}
		else {
			{
				'0': a
			}
		}
	}
}

// default_to returns `value` when `a` is `Null`.
pub fn (a Any) default_to(value Any) Any {
	return match a {
		Null { value }
		else { a }
	}
}

// value queries a value from the current node using dotted keys and array indices.
pub fn (a Any) value(key string) Any {
	return a.value_opt(key) or { null }
}

// value_opt queries a value from the current node and returns an error when missing.
// A YAML key whose value is the explicit `null` literal returns that `Null`
// (it is not treated as missing); only an absent key or a non-traversable
// path raises an error.
pub fn (a Any) value_opt(key string) !Any {
	key_split := parse_dotted_key(key) or { return error('invalid dotted key') }
	return a.value_(a, key_split) or { error('no value for key') }
}

// value queries a value from the map.
pub fn (m map[string]Any) value(key string) Any {
	return Any(m).value(key)
}

// value queries a value from the array.
pub fn (a []Any) value(key string) Any {
	return Any(a).value(key)
}

// as_strings returns the contents of the array as `[]string`.
pub fn (a []Any) as_strings() []string {
	mut out := []string{cap: a.len}
	for value in a {
		out << value.string()
	}
	return out
}

// as_strings returns the contents of the map as `map[string]string`.
pub fn (m map[string]Any) as_strings() map[string]string {
	mut out := map[string]string{}
	for key, value in m {
		out[key] = value.string()
	}
	return out
}

// to_json converts `Any` to JSON.
pub fn (a Any) to_json() string {
	mut sb := strings.new_builder(256)
	emit_any_as_json(mut sb, a)
	return sb.str()
}

// to_yaml converts `Any` to YAML.
pub fn (a Any) to_yaml() string {
	mut sb := strings.new_builder(256)
	emit_yaml_any(mut sb, a, 0)
	return sb.str()
}

// to_yaml converts a YAML array to YAML text.
pub fn (a []Any) to_yaml() string {
	return Any(a).to_yaml()
}

// to_yaml converts a YAML map to YAML text.
pub fn (m map[string]Any) to_yaml() string {
	return Any(m).to_yaml()
}
